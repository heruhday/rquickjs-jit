#include "quickjs.bind.h"
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint8_t *data;
    size_t len;
    size_t cap;
    bool little_endian;
    bool intern_strings;
    char **string_table;
    size_t *string_lengths;
    size_t string_count;
    size_t string_cap;
} MsgpackBuffer;

static void mp_free_string_table(MsgpackBuffer *b) {
    size_t i;
    for (i = 0; i < b->string_count; i++) free(b->string_table[i]);
    free(b->string_table);
    free(b->string_lengths);
}

static int mp_intern_string(MsgpackBuffer *b, const char *str, size_t len,
                            uint32_t *index) {
    size_t i;
    for (i = 0; i < b->string_count; i++)
        if (b->string_lengths[i] == len &&
            !memcmp(b->string_table[i], str, len)) {
            *index = (uint32_t)i;
            return 1;
        }
    if (b->string_count > 0xffffffffU) return -1;
    if (b->string_count == b->string_cap) {
        size_t cap = b->string_cap ? b->string_cap * 2 : 32;
        char **strings = (char **)malloc(cap * sizeof(*strings));
        size_t *lengths = (size_t *)malloc(cap * sizeof(*lengths));
        if (!strings || !lengths) {
            free(strings);
            free(lengths);
            return -1;
        }
        if (b->string_count) {
            memcpy(strings, b->string_table,
                   b->string_count * sizeof(*strings));
            memcpy(lengths, b->string_lengths,
                   b->string_count * sizeof(*lengths));
        }
        free(b->string_table);
        free(b->string_lengths);
        b->string_table = strings;
        b->string_lengths = lengths;
        b->string_cap = cap;
    }
    b->string_table[b->string_count] = (char *)malloc(len ? len : 1);
    if (!b->string_table[b->string_count]) return -1;
    memcpy(b->string_table[b->string_count], str, len);
    b->string_lengths[b->string_count] = len;
    *index = (uint32_t)b->string_count++;
    return 0;
}

static int mp_host_little(void) {
    uint16_t value = 1;
    return *((const uint8_t *)&value);
}

static int mp_reserve(MsgpackBuffer *b, size_t extra) {
    size_t need = b->len + extra;
    if (need <= b->cap) return 0;
    size_t cap = b->cap ? b->cap : 128;
    while (cap < need) {
        if (cap > (size_t)-1 / 2) return -1;
        cap *= 2;
    }
    uint8_t *data = (uint8_t *)realloc(b->data, cap);
    if (!data) return -1;
    b->data = data;
    b->cap = cap;
    return 0;
}

static int mp_byte(MsgpackBuffer *b, uint8_t value) {
    if (mp_reserve(b, 1)) return -1;
    b->data[b->len++] = value;
    return 0;
}

static int mp_bytes(MsgpackBuffer *b, const uint8_t *data, size_t len) {
    if (mp_reserve(b, len)) return -1;
    memcpy(b->data + b->len, data, len);
    b->len += len;
    return 0;
}

static int mp_u16(MsgpackBuffer *b, uint16_t value) {
    uint8_t data[2] = {
        (uint8_t)(b->little_endian ? value : value >> 8),
        (uint8_t)(b->little_endian ? value >> 8 : value)
    };
    return mp_bytes(b, data, 2);
}

static int mp_u32(MsgpackBuffer *b, uint32_t value) {
    uint8_t data[4];
    if (b->little_endian) {
        data[0] = (uint8_t)value;
        data[1] = (uint8_t)(value >> 8);
        data[2] = (uint8_t)(value >> 16);
        data[3] = (uint8_t)(value >> 24);
    } else {
        data[0] = (uint8_t)(value >> 24);
        data[1] = (uint8_t)(value >> 16);
        data[2] = (uint8_t)(value >> 8);
        data[3] = (uint8_t)value;
    }
    return mp_bytes(b, data, 4);
}

static int mp_u64(MsgpackBuffer *b, uint64_t value) {
    uint8_t data[8];
    int i;
    for (i = 0; i < 8; i++)
        data[i] = (uint8_t)(value >> (b->little_endian ? 8 * i : 56 - 8 * i));
    return mp_bytes(b, data, 8);
}

static int mp_str(MsgpackBuffer *b, const char *str, size_t len) {
    if (b->intern_strings) {
        uint32_t index;
        int found = mp_intern_string(b, str, len, &index);
        if (found < 0) return -1;
        if (found) {
            if (mp_byte(b, 0xd6) || mp_byte(b, 4) || mp_u32(b, index))
                return -1;
            return 0;
        }
    }
    if (len < 32) {
        if (mp_byte(b, (uint8_t)(0xa0 | len))) return -1;
    } else if (len <= 0xff) {
        if (mp_byte(b, 0xd9) || mp_byte(b, (uint8_t)len)) return -1;
    } else if (len <= 0xffff) {
        if (mp_byte(b, 0xda) || mp_u16(b, (uint16_t)len)) return -1;
    } else {
        if (mp_byte(b, 0xdb) || mp_u32(b, (uint32_t)len)) return -1;
    }
    return mp_bytes(b, (const uint8_t *)str, len);
}

static int mp_value(JSContext *ctx, JSValueConst value, MsgpackBuffer *b);

static int mp_ext(JSContext *ctx, JSValueConst value, MsgpackBuffer *b,
                  int type, size_t element_size) {
    size_t offset = 0, len = 0, buffer_len = 0, i;
    JSValue buffer;
    uint8_t *data;
    int host_little;

    buffer = JS_GetTypedArrayBuffer(ctx, value, &offset, &len, NULL);
    if (JS_IsException(buffer))
        return -1;
    data = JS_GetArrayBuffer(ctx, &buffer_len, buffer);
    if ((!data && len) || offset > buffer_len || len > buffer_len - offset ||
        (element_size > 1 && len % element_size)) {
        JS_FreeValue(ctx, buffer);
        JS_ThrowTypeError(ctx, "invalid typed array");
        return -1;
    }
    if (len <= 0xff) {
        if (mp_byte(b, 0xc7) || mp_byte(b, (uint8_t)len)) goto fail;
    } else if (len <= 0xffff) {
        if (mp_byte(b, 0xc8) || mp_u16(b, (uint16_t)len)) goto fail;
    } else {
        if (len > 0xffffffffU || mp_byte(b, 0xc9) ||
            mp_u32(b, (uint32_t)len)) goto fail;
    }
    if (mp_byte(b, (uint8_t)type)) goto fail;
    host_little = mp_host_little();
    data += offset;
    if (element_size == 1 || host_little == b->little_endian) {
        if (mp_bytes(b, data, len)) goto fail;
    } else {
        for (i = 0; i < len; i += element_size) {
            size_t j;
            for (j = 0; j < element_size; j++)
                if (mp_byte(b, data[i + element_size - 1 - j])) goto fail;
        }
    }
    JS_FreeValue(ctx, buffer);
    return 0;
fail:
    JS_FreeValue(ctx, buffer);
    return -1;
}

static void mp_free_buffer(JSRuntime *rt, void *opaque, void *ptr) {
    (void)rt;
    (void)opaque;
    free(ptr);
}

static int mp_value(JSContext *ctx, JSValueConst value, MsgpackBuffer *b) {
    int tag = JS_VALUE_GET_TAG(value);
    if (tag == JS_TAG_NULL || tag == JS_TAG_UNDEFINED)
        return mp_byte(b, 0xc0);
    if (tag == JS_TAG_BOOL)
        return mp_byte(b, JS_VALUE_GET_BOOL(value) ? 0xc3 : 0xc2);
    if (tag == JS_TAG_INT) {
        int32_t v = JS_VALUE_GET_INT(value);
        if (v >= 0 && v < 128) return mp_byte(b, (uint8_t)v);
        if (v < 0 && v >= -32)
            return mp_byte(b, (uint8_t)(0xe0 | (v + 32)));
        if (v >= 0 && v <= 0xff)
            return mp_byte(b, 0xcc) || mp_byte(b, (uint8_t)v);
        if (v < 0 && v >= -128)
            return mp_byte(b, 0xd0) || mp_byte(b, (uint8_t)v);
        if (v >= -32768) return mp_byte(b, 0xd1) || mp_u16(b, (uint16_t)(int16_t)v);
        return mp_byte(b, 0xd2) || mp_u32(b, (uint32_t)v);
    }
    if (JS_TAG_IS_FLOAT64(tag)) {
        double d = JS_VALUE_GET_FLOAT64(value);
        union { double d; uint64_t u; } bits = {d};
        return mp_byte(b, 0xcb) || mp_u64(b, bits.u);
    }
    if (tag == JS_TAG_STRING) {
        size_t len;
        const char *str = JS_ToCStringLen2(ctx, &len, value, 0);
        int rc;
        if (!str) return -1;
        rc = mp_str(b, str, len);
        JS_FreeCString(ctx, str);
        return rc;
    }
    if (JS_IsArrayBuffer(value)) {
        JS_ThrowTypeError(ctx, "binary and extension values are not supported");
        return -1;
    }
    {
        int typed_array_type = JS_GetTypedArrayType(value);
        if (typed_array_type >= 0) {
            if (typed_array_type == JS_TYPED_ARRAY_UINT8)
                return mp_ext(ctx, value, b, 1, 1);
            if (typed_array_type == JS_TYPED_ARRAY_INT32)
                return mp_ext(ctx, value, b, 2, 4);
            if (typed_array_type == JS_TYPED_ARRAY_FLOAT64)
                return mp_ext(ctx, value, b, 3, 8);
            JS_ThrowTypeError(ctx, "unsupported typed array");
            return -1;
        }
    }
    if (JS_IsArray(value)) {
        int64_t length;
        int64_t i;
        if (JS_GetLength(ctx, value, &length) < 0 || length < 0 ||
            (uint64_t)length > 0xffffffffU) return -1;
        if (length < 16) {
            if (mp_byte(b, (uint8_t)(0x90 | length))) return -1;
        } else if (length <= 0xffff) {
            if (mp_byte(b, 0xdc) || mp_u16(b, (uint16_t)length)) return -1;
        } else if (mp_byte(b, 0xdd) || mp_u32(b, (uint32_t)length)) return -1;
        for (i = 0; i < length; i++) {
            JSValue item = JS_GetPropertyUint32(ctx, value, (uint32_t)i);
            int rc = JS_IsException(item) ? -1 : mp_value(ctx, item, b);
            JS_FreeValue(ctx, item);
            if (rc) return -1;
        }
        return 0;
    }
    if (JS_IsObject(value)) {
        JSPropertyEnum *props = NULL;
        uint32_t count = 0, i;
        int rc = JS_GetOwnPropertyNames(ctx, &props, &count, value,
                                        JS_GPN_STRING_MASK | JS_GPN_ENUM_ONLY);
        if (rc < 0 || count > 0xffffffffU) return -1;
        if (count < 16) {
            if (mp_byte(b, (uint8_t)(0x80 | count))) rc = -1;
        } else if (count <= 0xffff) {
            if (mp_byte(b, 0xde) || mp_u16(b, (uint16_t)count)) rc = -1;
        } else if (mp_byte(b, 0xdf) || mp_u32(b, count)) rc = -1;
        for (i = 0; !rc && i < count; i++) {
            size_t len;
            const char *key = JS_AtomToCString(ctx, props[i].atom);
            JSValue item;
            if (!key) { rc = -1; break; }
            len = strlen(key);
            rc = mp_str(b, key, len);
            JS_FreeCString(ctx, key);
            item = JS_GetProperty(ctx, value, props[i].atom);
            if (!rc && JS_IsException(item)) rc = -1;
            if (!rc) rc = mp_value(ctx, item, b);
            JS_FreeValue(ctx, item);
        }
        JS_FreePropertyEnum(ctx, props, count);
        return rc;
    }
    JS_ThrowTypeError(ctx, "unsupported JavaScript value");
    return -1;
}

typedef struct {
    const uint8_t *data;
    size_t len;
    size_t pos;
    bool little_endian;
    bool intern_strings;
    JSValue *string_table;
    size_t string_count;
    size_t string_cap;
} MsgpackReader;

static void mp_free_reader_table(JSContext *ctx, MsgpackReader *r) {
    size_t i;
    for (i = 0; i < r->string_count; i++) JS_FreeValue(ctx, r->string_table[i]);
    free(r->string_table);
}

static int mp_reader_add_string(JSContext *ctx, MsgpackReader *r, JSValue value) {
    if (!r->intern_strings) return 0;
    if (r->string_count == r->string_cap) {
        size_t cap = r->string_cap ? r->string_cap * 2 : 32;
        JSValue *table = (JSValue *)malloc(cap * sizeof(*table));
        if (!table) return JS_ThrowOutOfMemory(ctx), -1;
        if (r->string_count)
            memcpy(table, r->string_table, r->string_count * sizeof(*table));
        free(r->string_table);
        r->string_table = table;
        r->string_cap = cap;
    }
    r->string_table[r->string_count++] = JS_DupValue(ctx, value);
    return 0;
}

static int mp_read(MsgpackReader *r, uint8_t *out) {
    if (r->pos >= r->len) return -1;
    *out = r->data[r->pos++];
    return 0;
}

static int mp_read_n(MsgpackReader *r, uint8_t *out, size_t len) {
    if (len > r->len - r->pos) return -1;
    memcpy(out, r->data + r->pos, len);
    r->pos += len;
    return 0;
}

static int mp_len(MsgpackReader *r, uint8_t marker, uint32_t *out, int kind) {
    uint8_t data[4];
    if (kind == 0 && (marker & 0xe0) == 0xa0) { *out = marker & 31; return 0; }
    if (kind == 1 && (marker & 0xf0) == 0x90) { *out = marker & 15; return 0; }
    if (kind == 2 && (marker & 0xf0) == 0x80) { *out = marker & 15; return 0; }
    if ((kind == 0 && marker == 0xd9) || (kind == 1 && marker == 0xdc) ||
        (kind == 2 && marker == 0xde)) {
        if (kind == 0) {
            if (mp_read_n(r, data, 1)) return -1;
            *out = data[0];
        } else {
            if (mp_read_n(r, data, 2)) return -1;
            *out = r->little_endian
                ? ((uint32_t)data[1] << 8 | data[0])
                : ((uint32_t)data[0] << 8 | data[1]);
        }
        return 0;
    }
    if ((kind == 0 && marker == 0xda) || (kind == 1 && marker == 0xdd) ||
        (kind == 2 && marker == 0xdf)) {
        if (kind == 0) {
            if (mp_read_n(r, data, 2)) return -1;
            *out = r->little_endian
                ? ((uint32_t)data[1] << 8 | data[0])
                : ((uint32_t)data[0] << 8 | data[1]);
        } else {
            if (mp_read_n(r, data, 4)) return -1;
            *out = r->little_endian
                ? ((uint32_t)data[3] << 24) | ((uint32_t)data[2] << 16) |
                  ((uint32_t)data[1] << 8) | data[0]
                : ((uint32_t)data[0] << 24) | ((uint32_t)data[1] << 16) |
                  ((uint32_t)data[2] << 8) | data[3];
        }
        return 0;
    }
    if (kind == 0 && marker == 0xdb) {
        if (mp_read_n(r, data, 4)) return -1;
        *out = r->little_endian
            ? ((uint32_t)data[3] << 24) | ((uint32_t)data[2] << 16) |
              ((uint32_t)data[1] << 8) | data[0]
            : ((uint32_t)data[0] << 24) | ((uint32_t)data[1] << 16) |
              ((uint32_t)data[2] << 8) | data[3];
        return 0;
    }
    return -1;
}

static JSValue mp_decode_ext(JSContext *ctx, MsgpackReader *r, uint32_t len,
                             int type) {
    size_t element_size;
    int array_type;
    uint8_t *copy;
    size_t i;
    int host_little;
    JSValue buffer, result, args[3];

    if (type == 1) {
        element_size = 1;
        array_type = JS_TYPED_ARRAY_UINT8;
    } else if (type == 2) {
        element_size = 4;
        array_type = JS_TYPED_ARRAY_INT32;
    } else if (type == 3) {
        element_size = 8;
        array_type = JS_TYPED_ARRAY_FLOAT64;
    } else {
        return JS_ThrowTypeError(ctx, "unsupported MessagePack extension type");
    }
    if (len % element_size || len > r->len - r->pos)
        return JS_ThrowTypeError(ctx, "invalid MessagePack extension");
    copy = (uint8_t *)malloc(len ? len : 1);
    if (!copy)
        return JS_ThrowOutOfMemory(ctx);
    memcpy(copy, r->data + r->pos, len);
    r->pos += len;
    host_little = mp_host_little();
    if (element_size > 1 && host_little != r->little_endian) {
        for (i = 0; i < len; i += element_size) {
            size_t j;
            for (j = 0; j < element_size / 2; j++) {
                uint8_t t = copy[i + j];
                copy[i + j] = copy[i + element_size - 1 - j];
                copy[i + element_size - 1 - j] = t;
            }
        }
    }
    buffer = JS_NewArrayBufferCopy(ctx, copy, len);
    free(copy);
    if (JS_IsException(buffer))
        return buffer;
    args[0] = buffer;
    args[1] = JS_UNDEFINED;
    args[2] = JS_UNDEFINED;
    result = JS_NewTypedArray(ctx, 3, args, array_type);
    JS_FreeValue(ctx, buffer);
    return result;
}

static JSValue mp_decode_value(JSContext *ctx, MsgpackReader *r) {
    uint8_t m, data[8];
    uint32_t len;
    if (mp_read(r, &m)) return JS_ThrowTypeError(ctx, "invalid MessagePack data");
    if (m == 0xc0) return JS_NULL;
    if (m == 0xc2 || m == 0xc3) return JS_NewBool(ctx, m == 0xc3);
    if (m <= 0x7f) return JS_NewInt32(ctx, m);
    if (m >= 0xe0) return JS_NewInt32(ctx, (int8_t)m);
    if (m >= 0xa0 && m <= 0xbf) len = m & 31;
    else if (m == 0xd9 || m == 0xda || m == 0xdb) {
        if (mp_len(r, m, &len, 0)) return JS_ThrowTypeError(ctx, "invalid MessagePack string");
    } else if (m == 0xcc || m == 0xcd || m == 0xce || m == 0xcf ||
               m == 0xd0 || m == 0xd1 || m == 0xd2 || m == 0xd3 ||
               m == 0xca || m == 0xcb) {
        int n = (m == 0xcc || m == 0xd0) ? 1 : (m == 0xcd || m == 0xd1) ? 2 :
                (m == 0xce || m == 0xd2 || m == 0xca) ? 4 : 8;
        if (mp_read_n(r, data, n)) return JS_ThrowTypeError(ctx, "invalid MessagePack number");
        if (m == 0xca) {
            uint32_t u = r->little_endian
                ? ((uint32_t)data[3] << 24) | ((uint32_t)data[2] << 16) |
                  ((uint32_t)data[1] << 8) | data[0]
                : ((uint32_t)data[0] << 24) | ((uint32_t)data[1] << 16) |
                  ((uint32_t)data[2] << 8) | data[3];
            union { uint32_t u; float f; } x = {u};
            return JS_NewFloat64(ctx, x.f);
        }
        if (m == 0xcb) {
            uint64_t u = 0;
            int i;
            union { uint64_t u; double d; } x;
            if (r->little_endian)
                for (i = 7; i >= 0; i--) u = (u << 8) | data[i];
            else
                for (i = 0; i < 8; i++) u = (u << 8) | data[i];
            x.u = u;
            return JS_NewFloat64(ctx, x.d);
        }
        {
            uint64_t u = 0;
            int i;
            if (r->little_endian)
                for (i = n - 1; i >= 0; i--) u = (u << 8) | data[i];
            else
                for (i = 0; i < n; i++) u = (u << 8) | data[i];
            if (m == 0xd0) return JS_NewInt32(ctx, (int8_t)u);
            if (m == 0xd1) return JS_NewInt32(ctx, (int16_t)u);
            if (m == 0xd2) return JS_NewInt64(ctx, (int32_t)u);
            if (m == 0xd3) return JS_NewInt64(ctx, (int64_t)u);
            return JS_NewInt64(ctx, (int64_t)u);
        }
    } else if (m >= 0x90 && m <= 0x9f) {
        len = m & 15; goto array_value;
    } else if (m == 0xdc || m == 0xdd) {
        if (mp_len(r, m, &len, 1)) return JS_ThrowTypeError(ctx, "invalid MessagePack array");
        goto array_value;
    } else if (m >= 0x80 && m <= 0x8f) {
        len = m & 15; goto map_value;
    } else if (m == 0xde || m == 0xdf) {
        if (mp_len(r, m, &len, 2)) return JS_ThrowTypeError(ctx, "invalid MessagePack map");
        goto map_value;
    } else if (m == 0xd4 || m == 0xd5 || m == 0xd6 || m == 0xd7 || m == 0xd8 ||
               m == 0xc7 || m == 0xc8 || m == 0xc9) {
        int type;
        if (m == 0xd4) len = 1;
        else if (m == 0xd5) len = 2;
        else if (m == 0xd6) len = 4;
        else if (m == 0xd7) len = 8;
        else if (m == 0xd8) len = 16;
        else {
            uint8_t length_data[4];
            int length_bytes = m == 0xc7 ? 1 : m == 0xc8 ? 2 : 4;
            if (mp_read_n(r, length_data, length_bytes))
                return JS_ThrowTypeError(ctx, "invalid MessagePack extension");
            if (length_bytes == 1) len = length_data[0];
            else if (length_bytes == 2)
                len = r->little_endian
                    ? ((uint32_t)length_data[1] << 8) | length_data[0]
                    : ((uint32_t)length_data[0] << 8) | length_data[1];
            else
                len = r->little_endian
                    ? ((uint32_t)length_data[3] << 24) |
                      ((uint32_t)length_data[2] << 16) |
                      ((uint32_t)length_data[1] << 8) | length_data[0]
                    : ((uint32_t)length_data[0] << 24) |
                      ((uint32_t)length_data[1] << 16) |
                      ((uint32_t)length_data[2] << 8) | length_data[3];
        }
        if (mp_read(r, &data[0]))
            return JS_ThrowTypeError(ctx, "invalid MessagePack extension");
        if (r->intern_strings && (int8_t)data[0] == 4) {
            uint32_t index;
            if (len != 4 || mp_read_n(r, data, 4))
                return JS_ThrowTypeError(ctx, "invalid interned string reference");
            index = r->little_endian
                ? ((uint32_t)data[3] << 24) | ((uint32_t)data[2] << 16) |
                  ((uint32_t)data[1] << 8) | data[0]
                : ((uint32_t)data[0] << 24) | ((uint32_t)data[1] << 16) |
                  ((uint32_t)data[2] << 8) | data[3];
            if (index >= r->string_count)
                return JS_ThrowTypeError(ctx, "invalid interned string reference");
            return JS_DupValue(ctx, r->string_table[index]);
        }
        return mp_decode_ext(ctx, r, len, (int8_t)data[0]);
    } else {
        return JS_ThrowTypeError(ctx, "unsupported binary or extension MessagePack value");
    }
    {
        JSValue result;
        if (len > r->len - r->pos)
            return JS_ThrowTypeError(ctx, "invalid MessagePack string");
        result = JS_NewStringLen(ctx, (const char *)(r->data + r->pos), len);
        r->pos += len;
        if (!JS_IsException(result) && mp_reader_add_string(ctx, r, result)) {
            JS_FreeValue(ctx, result);
            return JS_EXCEPTION;
        }
        return result;
    }
array_value:
    { JSValue array = JS_NewArray(ctx); uint32_t i; if (JS_IsException(array)) return array; for (i=0;i<len;i++) { JSValue item=mp_decode_value(ctx,r); if(JS_IsException(item)||JS_SetPropertyUint32(ctx,array,i,item)<0){JS_FreeValue(ctx,item);JS_FreeValue(ctx,array);return JS_EXCEPTION;} } return array; }
map_value:
    {
        JSValue object = JS_NewObject(ctx);
        uint32_t i;
        if (JS_IsException(object))
            return object;
        for (i = 0; i < len; i++) {
            JSValue key = mp_decode_value(ctx, r);
            JSValue val;
            const char *s;
            if (JS_IsException(key) || JS_VALUE_GET_TAG(key) != JS_TAG_STRING) {
                JS_FreeValue(ctx, key);
                JS_FreeValue(ctx, object);
                return JS_ThrowTypeError(ctx, "map keys must be strings");
            }
            s = JS_ToCString(ctx, key);
            val = mp_decode_value(ctx, r);
            if (!s || JS_IsException(val) ||
                JS_SetPropertyStr(ctx, object, s, val) < 0) {
                if (s)
                    JS_FreeCString(ctx, s);
                JS_FreeValue(ctx, key);
                JS_FreeValue(ctx, val);
                JS_FreeValue(ctx, object);
                return JS_EXCEPTION;
            }
            JS_FreeCString(ctx, s);
            JS_FreeValue(ctx, key);
        }
        return object;
    }
}

static JSValue mp_encode_value(JSContext *ctx, int argc, JSValueConst *argv,
                               bool little_endian, bool intern_strings) {
    MsgpackBuffer b = {0};
    JSValue result;
    b.little_endian = little_endian;
    b.intern_strings = intern_strings;
    if (argc < 1 || mp_value(ctx, argv[0], &b)) {
        free(b.data);
        mp_free_string_table(&b);
        return JS_EXCEPTION;
    }
    result = JS_NewUint8Array(ctx, b.data, b.len, mp_free_buffer, NULL, false);
    if (JS_IsException(result)) free(b.data);
    mp_free_string_table(&b);
    return result;
}

static JSValue mp_encode(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    return mp_encode_value(ctx, argc, argv, false, false);
}

static JSValue mp_encode_le(JSContext *ctx, JSValueConst this_val, int argc,
                            JSValueConst *argv) {
    (void)this_val;
    return mp_encode_value(ctx, argc, argv, true, false);
}

static JSValue mp_encode_intern(JSContext *ctx, JSValueConst this_val, int argc,
                                JSValueConst *argv) {
    (void)this_val;
    return mp_encode_value(ctx, argc, argv, false, true);
}

static JSValue mp_encode_le_intern(JSContext *ctx, JSValueConst this_val, int argc,
                                   JSValueConst *argv) {
    (void)this_val;
    return mp_encode_value(ctx, argc, argv, true, true);
}

static JSValue mp_decode_value_from_arg(JSContext *ctx, int argc,
                                        JSValueConst *argv, bool little_endian,
                                        bool intern_strings) {
    size_t offset = 0, length = 0, buffer_length = 0; uint8_t *data; JSValue buffer = JS_UNDEFINED; MsgpackReader r = {0}; JSValue result;
    if (argc < 1) return JS_ThrowTypeError(ctx, "MessagePack data is required");
    data = JS_GetArrayBuffer(ctx, &length, argv[0]);
    if (!data && JS_GetTypedArrayType(argv[0]) >= 0) { buffer = JS_GetTypedArrayBuffer(ctx, argv[0], &offset, &length, NULL); if (JS_IsException(buffer)) return buffer; buffer_length = length; data = JS_GetArrayBuffer(ctx, &buffer_length, buffer); data += offset; }
    if (!data && JS_IsArray(argv[0])) {
        int64_t array_length;
        uint8_t *copy;
        int64_t i;
        if (JS_GetLength(ctx, argv[0], &array_length) < 0 || array_length < 0) return JS_EXCEPTION;
        copy = (uint8_t *)malloc((size_t)array_length ? (size_t)array_length : 1);
        if (!copy) return JS_ThrowOutOfMemory(ctx);
        for (i = 0; i < array_length; i++) {
            JSValue item = JS_GetPropertyUint32(ctx, argv[0], (uint32_t)i);
            uint32_t byte;
            if (JS_IsException(item) || JS_ToUint32(ctx, &byte, item) < 0 || byte > 255) {
                JS_FreeValue(ctx, item);
                free(copy);
                return JS_ThrowTypeError(ctx, "invalid MessagePack byte array");
            }
            copy[i] = (uint8_t)byte;
            JS_FreeValue(ctx, item);
        }
        r.data = copy; r.len = (size_t)array_length; r.pos = 0;
        r.little_endian = little_endian;
        r.intern_strings = intern_strings;
        result = mp_decode_value(ctx, &r);
        mp_free_reader_table(ctx, &r);
        free(copy);
        return result;
    }
    if (!data) return JS_ThrowTypeError(ctx, "MessagePack data must be an ArrayBuffer or typed array");
    r.data=data; r.len=length; r.pos=0; r.little_endian = little_endian;
    r.intern_strings = intern_strings;
    result=mp_decode_value(ctx,&r);
    mp_free_reader_table(ctx, &r);
    JS_FreeValue(ctx,buffer);
    return result;
}

static JSValue mp_decode(JSContext *ctx, JSValueConst this_val, int argc,
                         JSValueConst *argv) {
    (void)this_val;
    return mp_decode_value_from_arg(ctx, argc, argv, false, false);
}

static JSValue mp_decode_le(JSContext *ctx, JSValueConst this_val, int argc,
                            JSValueConst *argv) {
    (void)this_val;
    return mp_decode_value_from_arg(ctx, argc, argv, true, false);
}

static JSValue mp_decode_intern(JSContext *ctx, JSValueConst this_val, int argc,
                                JSValueConst *argv) {
    (void)this_val;
    return mp_decode_value_from_arg(ctx, argc, argv, false, true);
}

static JSValue mp_decode_le_intern(JSContext *ctx, JSValueConst this_val, int argc,
                                   JSValueConst *argv) {
    (void)this_val;
    return mp_decode_value_from_arg(ctx, argc, argv, true, true);
}

int qjs_msgpack_init(JSContext *ctx) {
    JSValue module = JS_NewObject(ctx);
    JSValue module_le = JS_NewObject(ctx);
    JSValue module_intern = JS_NewObject(ctx);
    JSValue module_le_intern = JS_NewObject(ctx);
    JSValue global;
    if (JS_IsException(module) || JS_IsException(module_le) ||
        JS_IsException(module_intern) || JS_IsException(module_le_intern)) {
        JS_FreeValue(ctx, module);
        JS_FreeValue(ctx, module_le);
        JS_FreeValue(ctx, module_intern);
        JS_FreeValue(ctx, module_le_intern);
        return -1;
    }
    global = JS_GetGlobalObject(ctx);
    if (JS_SetPropertyStr(ctx, module, "encode", JS_NewCFunction(ctx, mp_encode, "encode", 1)) < 0 ||
        JS_SetPropertyStr(ctx, module, "decode", JS_NewCFunction(ctx, mp_decode, "decode", 1)) < 0 ||
        JS_SetPropertyStr(ctx, module_le, "encode", JS_NewCFunction(ctx, mp_encode_le, "encode", 1)) < 0 ||
        JS_SetPropertyStr(ctx, module_le, "decode", JS_NewCFunction(ctx, mp_decode_le, "decode", 1)) < 0 ||
        JS_SetPropertyStr(ctx, module_intern, "encode", JS_NewCFunction(ctx, mp_encode_intern, "encode", 1)) < 0 ||
        JS_SetPropertyStr(ctx, module_intern, "decode", JS_NewCFunction(ctx, mp_decode_intern, "decode", 1)) < 0 ||
        JS_SetPropertyStr(ctx, module_le_intern, "encode", JS_NewCFunction(ctx, mp_encode_le_intern, "encode", 1)) < 0 ||
        JS_SetPropertyStr(ctx, module_le_intern, "decode", JS_NewCFunction(ctx, mp_decode_le_intern, "decode", 1)) < 0 ||
        JS_SetPropertyStr(ctx, global, "msgpack", module) < 0 ||
        JS_SetPropertyStr(ctx, global, "msgpackLE", module_le) < 0 ||
        JS_SetPropertyStr(ctx, global, "msgpackIntern", module_intern) < 0 ||
        JS_SetPropertyStr(ctx, global, "msgpackLEIntern", module_le_intern) < 0) {
        JS_FreeValue(ctx, global);
        return -1;
    }
    JS_FreeValue(ctx, global);
    return 0;
}

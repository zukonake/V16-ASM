#pragma once

#include <stdint.h>

const uint8_t  vx_ver_major = 1;
const uint8_t  vx_ver_minor = 0;
const uint16_t vx_ver_patch = 0;

uint32_t get_vx_ver();

const uint8_t ERR_VAL = 0xFF;

/* Op is the Opcode
 * Mk is the Mode Kind (type)
 *
 * v stands for raw integer value
 * c stands for processor cycle cost
 * s stands for size, i.e. how many arguments does it take
 * str is the string representation
 */

typedef struct
{
    uint8_t v;
    uint8_t c;
    uint8_t s;
    const char *str;
} Op;

typedef struct
{
    uint8_t v;
    uint8_t c;
    uint8_t s;
    const char *str;
} Mk;

Op          get_op(uint8_t op);
Mk          get_mk(uint8_t mk);

uint8_t     asm_op(const char *str);
uint8_t     asm_mk(const char *str);

uint8_t     op_to_c(uint8_t op);
uint8_t     op_to_s(uint8_t op);
const char *op_to_str(uint8_t op);
uint8_t     mk_to_c(uint8_t mk);
uint8_t     mk_to_s(uint8_t mk);
const char *mk_to_str(uint8_t mk);

#define DEF_OP(x, v, c, s) const Op OP_##x = {v, c, s, #x};
#define DEF_MK(x, v, c, s) const Mk MK_##x = {v, c, s, #x};

DEF_OP(NOP , 0x00, 1, 0);
DEF_OP(JUMP, 0x01, 1, 1);
DEF_OP(TERM, 0x02, 1, 1);
DEF_OP(CALL, 0x03, 2, 1);
DEF_OP(RET , 0x04, 1, 0);
DEF_OP(PUSH, 0x05, 1, 1);
DEF_OP(POP , 0x06, 1, 1);
DEF_OP(MOVE, 0x10, 1, 2);
DEF_OP(COPY, 0x11, 2, 2);
DEF_OP(SWAP, 0x12, 2, 2);
DEF_OP(IFEQ, 0x20, 1, 2);
DEF_OP(IFNQ, 0x21, 1, 2);
DEF_OP(IFGT, 0x22, 2, 2);
DEF_OP(IFLT, 0x23, 2, 2);
DEF_OP(IFGQ, 0x24, 2, 2);
DEF_OP(IFLQ, 0x25, 2, 2);
DEF_OP(NEG , 0x30, 1, 2);
DEF_OP(OR  , 0x31, 1, 2);
DEF_OP(AND , 0x32, 1, 2);
DEF_OP(XOR , 0x33, 1, 2);
DEF_OP(RSHF, 0x34, 1, 2);
DEF_OP(LSHF, 0x35, 1, 2);
DEF_OP(SWPB, 0x36, 2, 1);
DEF_OP(ADD , 0x40, 2, 2);
DEF_OP(SUB , 0x41, 2, 2);
DEF_OP(MUL , 0x42, 3, 2);
DEF_OP(DIV , 0x43, 3, 2);
DEF_OP(MOD , 0x44, 4, 2);
DEF_OP(ADBI, 0x50, 1, 2);
DEF_OP(ADBO, 0x51, 1, 2);
DEF_OP(ADWI, 0x52, 2, 2);
DEF_OP(ADWO, 0x53, 2, 2);

DEF_MK(L, 00, 0, 1);
DEF_MK(R, 01, 0, 1);
DEF_MK(A, 02, 2, 1);
DEF_MK(B, 03, 2, 1);
DEF_MK(P, 04, 2, 1);
DEF_MK(N, 05, 2, 1);
DEF_MK(C, 06, 0, 0);

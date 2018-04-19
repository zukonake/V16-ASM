#include <string.h>
//
#include <vortex.h>

uint32_t get_vx_ver()
{
    return ((uint32_t)vx_ver_major << 24) |
           ((uint32_t)vx_ver_minor << 16) |
           ((uint32_t)vx_ver_patch);
}

Op get_op(uint8_t op)
{
    #define GET_OP(x) if(op == OP_##x.v) return OP_##x; else
    GET_OP(NOP)
    GET_OP(JUMP)
    GET_OP(TERM)
    GET_OP(CALL)
    GET_OP(RET)
    GET_OP(PUSH)
    GET_OP(POP)
    GET_OP(MOVE)
    GET_OP(COPY)
    GET_OP(SWAP)
    GET_OP(IFEQ)
    GET_OP(IFNQ)
    GET_OP(IFGT)
    GET_OP(IFLT)
    GET_OP(IFGQ)
    GET_OP(IFLQ)
    GET_OP(NEG)
    GET_OP(OR)
    GET_OP(AND)
    GET_OP(XOR)
    GET_OP(RSHF)
    GET_OP(LSHF)
    GET_OP(SWPB)
    GET_OP(ADD)
    GET_OP(SUB)
    GET_OP(MUL)
    GET_OP(DIV)
    GET_OP(MOD)
    GET_OP(ADBI)
    GET_OP(ADBO)
    GET_OP(ADWI)
    GET_OP(ADWO)
    return (Op) {ERR_VAL, ERR_VAL, ERR_VAL, NULL};
}

Mk get_mk(uint8_t mk)
{
    #define GET_MK(x) if(mk == MK_##x.v) return MK_##x; else
    GET_MK(L);
    GET_MK(R);
    GET_MK(A);
    GET_MK(B);
    GET_MK(P);
    GET_MK(N);
    GET_MK(C);
    return (Mk) {ERR_VAL, ERR_VAL, ERR_VAL, NULL};
}

uint8_t asm_op(const char *str)
{
    #define ASM_OP(x) if(strcmp(str, #x) == 0) return OP_##x.v; else
    ASM_OP(NOP)
    ASM_OP(JUMP)
    ASM_OP(TERM)
    ASM_OP(CALL)
    ASM_OP(RET)
    ASM_OP(PUSH)
    ASM_OP(POP)
    ASM_OP(MOVE)
    ASM_OP(COPY)
    ASM_OP(SWAP)
    ASM_OP(IFEQ)
    ASM_OP(IFNQ)
    ASM_OP(IFGT)
    ASM_OP(IFLT)
    ASM_OP(IFGQ)
    ASM_OP(IFLQ)
    ASM_OP(NEG)
    ASM_OP(OR)
    ASM_OP(AND)
    ASM_OP(XOR)
    ASM_OP(RSHF)
    ASM_OP(LSHF)
    ASM_OP(SWPB)
    ASM_OP(ADD)
    ASM_OP(SUB)
    ASM_OP(MUL)
    ASM_OP(DIV)
    ASM_OP(MOD)
    ASM_OP(ADBI)
    ASM_OP(ADBO)
    ASM_OP(ADWI)
    ASM_OP(ADWO)
    return ERR_VAL;
}

uint8_t asm_mk(const char *str)
{
    #define ASM_MK(x) if(strcmp(str, #x) == 0) return MK_##x.v; else
    ASM_MK(L);
    ASM_MK(R);
    ASM_MK(A);
    ASM_MK(B);
    ASM_MK(P);
    ASM_MK(N);
    ASM_MK(C);
    return ERR_VAL;
}

uint8_t op_to_c(uint8_t op)
{
    return get_op(op).c;
}

uint8_t op_to_s(uint8_t op)
{
    return get_op(op).s;
}

const char *op_to_str(uint8_t op)
{
    return get_op(op).str;
}

uint8_t mk_to_c(uint8_t mk)
{
    return get_op(mk).c;
}

uint8_t mk_to_s(uint8_t mk)
{
    return get_mk(mk).s;
}

const char *mk_to_str(uint8_t mk)
{
    return get_mk(mk).str;
}

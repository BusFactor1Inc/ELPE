/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#if FOSSIL_HARDENED_SHA1
typedef struct SHA1_CTX SHA1_CTX;
int SHA1DCFinal(unsigned char[20],SHA1_CTX *);
#endif
#if FOSSIL_HARDENED_SHA1  /* Only do this code if requested */
int SHA1DCFinal(unsigned char output[20],SHA1_CTX *ctx);
#endif
#if FOSSIL_HARDENED_SHA1
void SHA1DCUpdate(SHA1_CTX *,const unsigned char *,unsigned);
#endif
#if FOSSIL_HARDENED_SHA1  /* Only do this code if requested */
void SHA1DCUpdate(SHA1_CTX *ctx,const unsigned char *buf,unsigned len);
#endif
#if FOSSIL_HARDENED_SHA1
typedef void(*collision_block_callback)(uint64_t,const uint32_t *,const uint32_t *,const uint32_t *,const uint32_t *);
#endif
#if FOSSIL_HARDENED_SHA1  /* Only do this code if requested */
void SHA1DCSetCallback(SHA1_CTX *ctx,collision_block_callback callback);
void SHA1DCSetDetectReducedRoundCollision(SHA1_CTX *ctx,int reduced_round_coll);
void SHA1DCSetUseDetectColl(SHA1_CTX *ctx,int detect_coll);
void ubc_check(const uint32_t W[80],uint32_t dvmask[1]);
void SHA1DCSetUseUBC(SHA1_CTX *ctx,int ubc_check);
void SHA1DCSetSafeHash(SHA1_CTX *ctx,int safehash);
#endif
#if FOSSIL_HARDENED_SHA1
void SHA1DCInit(SHA1_CTX *);
#endif
#if FOSSIL_HARDENED_SHA1  /* Only do this code if requested */
void SHA1DCInit(SHA1_CTX *ctx);
void swap_bytes(uint32_t val[16]);
void sha1_process(SHA1_CTX *ctx,const uint32_t block[16]);
extern dv_info_t sha1_dvs[];
#endif
#if FOSSIL_HARDENED_SHA1
struct SHA1_CTX {
  uint64_t total;
  uint32_t ihv[5];
  unsigned char buffer[64];
  int bigendian;
  int found_collision;
  int safe_hash;
  int detect_coll;
  int ubc_check;
  int reduced_round_coll;
  collision_block_callback callback;

  uint32_t ihv1[5];
  uint32_t ihv2[5];
  uint32_t m1[80];
  uint32_t m2[80];
  uint32_t states[80][5];
};
#endif
#if FOSSIL_HARDENED_SHA1  /* Only do this code if requested */
extern sha1_recompression_type sha1_recompression_step[80];
extern SHA1_RECOMPRESS(0)SHA1_RECOMPRESS(1)SHA1_RECOMPRESS(2)SHA1_RECOMPRESS(3)SHA1_RECOMPRESS(4)SHA1_RECOMPRESS(5)SHA1_RECOMPRESS(6)SHA1_RECOMPRESS(7)SHA1_RECOMPRESS(8)SHA1_RECOMPRESS(9)SHA1_RECOMPRESS(10)SHA1_RECOMPRESS(11)SHA1_RECOMPRESS(12)SHA1_RECOMPRESS(13)SHA1_RECOMPRESS(14)SHA1_RECOMPRESS(15)SHA1_RECOMPRESS(16)SHA1_RECOMPRESS(17)SHA1_RECOMPRESS(18)SHA1_RECOMPRESS(19)SHA1_RECOMPRESS(20)SHA1_RECOMPRESS(21)SHA1_RECOMPRESS(22)SHA1_RECOMPRESS(23)SHA1_RECOMPRESS(24)SHA1_RECOMPRESS(25)SHA1_RECOMPRESS(26)SHA1_RECOMPRESS(27)SHA1_RECOMPRESS(28)SHA1_RECOMPRESS(29)SHA1_RECOMPRESS(30)SHA1_RECOMPRESS(31)SHA1_RECOMPRESS(32)SHA1_RECOMPRESS(33)SHA1_RECOMPRESS(34)SHA1_RECOMPRESS(35)SHA1_RECOMPRESS(36)SHA1_RECOMPRESS(37)SHA1_RECOMPRESS(38)SHA1_RECOMPRESS(39)SHA1_RECOMPRESS(40)SHA1_RECOMPRESS(41)SHA1_RECOMPRESS(42)SHA1_RECOMPRESS(43)SHA1_RECOMPRESS(44)SHA1_RECOMPRESS(45)SHA1_RECOMPRESS(46)SHA1_RECOMPRESS(47)SHA1_RECOMPRESS(48)SHA1_RECOMPRESS(49)SHA1_RECOMPRESS(50)SHA1_RECOMPRESS(51)SHA1_RECOMPRESS(52)SHA1_RECOMPRESS(53)SHA1_RECOMPRESS(54)SHA1_RECOMPRESS(55)SHA1_RECOMPRESS(56)SHA1_RECOMPRESS(57)SHA1_RECOMPRESS(58)SHA1_RECOMPRESS(59)SHA1_RECOMPRESS(60)SHA1_RECOMPRESS(61)SHA1_RECOMPRESS(62)SHA1_RECOMPRESS(63)SHA1_RECOMPRESS(64)SHA1_RECOMPRESS(65)SHA1_RECOMPRESS(66)SHA1_RECOMPRESS(67)SHA1_RECOMPRESS(68)SHA1_RECOMPRESS(69)SHA1_RECOMPRESS(70)SHA1_RECOMPRESS(71)SHA1_RECOMPRESS(72)SHA1_RECOMPRESS(73)SHA1_RECOMPRESS(74)SHA1_RECOMPRESS(75)SHA1_RECOMPRESS(76)SHA1_RECOMPRESS(77)SHA1_RECOMPRESS(78)SHA1_RECOMPRESS(79)sha1_recompression_type sha1_recompression_step[80];
void sha1_compression_states(uint32_t ihv[5],const uint32_t W[80],uint32_t states[80][5]);
void sha1_compression_states(uint32_t ihv[5],const uint32_t W[80],uint32_t states[80][5]);
void sha1_compression_W(uint32_t ihv[5],const uint32_t W[80]);
void sha1_compression_W(uint32_t ihv[5],const uint32_t W[80]);
void sha1_compression(uint32_t ihv[5],const uint32_t m[16]);
void sha1_compression(uint32_t ihv[5],const uint32_t m[16]);
void sha1_message_expansion(uint32_t W[80]);
void sha1_message_expansion(uint32_t W[80]);
#endif

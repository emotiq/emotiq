/* pbc-intf.c -- Interface between Lisp and PBC libs */
/* DM/Emotiq 03/18 */
/*
Copyright (c) 2018 Emotiq AG

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */

#include "pbc_intf.h"

// ---------------------------------------------------
// for initial interface testing...

extern "C"
long echo(long nel, char* msg_in, char* msg_out)
{
  memcpy(msg_out, msg_in, nel);
  return nel;
}

// ---------------------------------------------------

bool      init_flag = false;
pairing_t gPairing;
element_t gG1_gen, gG2_gen; // base generators for G1 and G2

extern "C"
long init_pairing(char* param_str, long nel, long* psize)
{
  long ans;
  
  if(init_flag)
    {
      element_clear(gG1_gen);
      element_clear(gG2_gen);
      pairing_clear(gPairing);
      init_flag = false;
    }
  ans = pairing_init_set_buf(gPairing, param_str, nel);
  if(0 == ans)
    {
      element_t z, pair;
      
      element_init_G1(gG1_gen, gPairing);
      element_init_G2(gG2_gen, gPairing);
      
      element_init_GT(pair,    gPairing); // archetypes for sizing info
      element_init_Zr(z,       gPairing);
      
      element_random(gG1_gen); // default random values
      element_random(gG2_gen);

      psize[0] = element_length_in_bytes_compressed(gG1_gen);
      psize[1] = element_length_in_bytes_compressed(gG2_gen);
      psize[2] = element_length_in_bytes(pair);
      psize[3] = element_length_in_bytes(z);

      element_clear(pair);
      element_clear(z);
      init_flag = true;
    }
  return ans;
}

extern "C"
long set_g2(unsigned char* pbuf)
{
  // Changing G2 generator invalidates all keying,
  // so remake default random key pair
  return element_from_bytes_compressed(gG2_gen, pbuf);
}

extern "C"
long set_g1(unsigned char* pbuf)
{
  return element_from_bytes_compressed(gG1_gen, pbuf);
}

// --------------------------------------------

extern "C"
void make_key_pair(unsigned char *pskey, unsigned char* ppkey,
		   unsigned char* phash, long nhash)
{
  element_t skey, pkey;
  element_init_Zr(skey, gPairing);
  element_init_G2(pkey, gPairing);
  
  element_from_hash(skey, phash, nhash);
  element_pow_zn(pkey, gG2_gen, skey);
  element_to_bytes(pskey, skey);
  element_to_bytes_compressed(ppkey, pkey);

  element_clear(skey);
  element_clear(pkey);
}
 
extern "C"
void sign_hash(unsigned char* psig, unsigned char* pskey,
	       unsigned char* phash, long nhash)
{
  element_t sig, skey;

  element_init_G1(sig,  gPairing);
  element_init_Zr(skey, gPairing);
  element_from_hash(sig, phash, nhash);
  element_from_bytes(skey, pskey);
  element_pow_zn(sig, sig, skey);
  element_to_bytes_compressed(psig, sig);
  element_clear(sig);
  element_clear(skey);
}

extern "C"
void make_public_subkey(unsigned char* abuf,
			unsigned char* pkey,
			unsigned char* phash_id, long nhash)
{
  element_t z;
  element_t gx, gp;
  
  element_init_Zr(z, gPairing);
  element_init_G2(gx, gPairing);
  element_init_G2(gp, gPairing);
  element_from_bytes_compressed(gp, pkey);
  element_from_hash(z, phash_id, nhash);
  element_pow_zn(gx, gG2_gen, z);
  element_mul(gp, gx, gp);
  element_to_bytes_compressed(abuf, gp); // ans is G2
  element_clear(z);
  element_clear(gx);
  element_clear(gp);
}

extern "C"
void make_secret_subkey(unsigned char* abuf,
			unsigned char* skey,
			unsigned char* phash_id, long nhash)
{
  element_t z, zs, s;

  element_init_Zr(z, gPairing);
  element_init_Zr(zs, gPairing);
  element_init_G1(s, gPairing);
  element_from_hash(z, phash_id, nhash);   // get ID
  element_from_bytes(zs, skey);            // user's secret key
  element_add(z, z, zs);
  element_invert(z, z);
  element_pow_zn(s, gG1_gen, z);
  element_to_bytes_compressed(abuf, s);    // ans is G1
  element_clear(z);
  element_clear(zs);
  element_clear(s);
}

extern "C"
void compute_pairing(unsigned char* gtbuf,
		     unsigned char* hbuf,
		     unsigned char* gbuf)
{
  element_t hh, gg, pair;

  element_init_G1(hh,   gPairing);
  element_init_G2(gg,   gPairing);
  element_init_GT(pair, gPairing);
  
  element_from_bytes_compressed(hh, hbuf);
  element_from_bytes_compressed(gg, gbuf);
  pairing_apply(pair, hh, gg, gPairing);
  element_to_bytes(gtbuf, pair);
  element_clear(pair);
  element_clear(hh);
  element_clear(gg);
}

extern "C"
void sakai_kasahara_encrypt(unsigned char* rbuf, // R result in G2
			    unsigned char* pbuf, // pairing result in GT
			    unsigned char* pkey, // public subkey in G2
			    unsigned char* phash, long nhash)
{
  element_t zr, gt, pk;

  /* pk, pkey is the public-subkey */
  /* phash, zr is the hash(ID || Tstamp || msg) */
  /* result R = zr*Psubkey */
  /* result pairing e(zr*U,Psubkey) = e(U,zr*Psubkey) */
  
  element_init_G2(pk, gPairing);
  element_init_Zr(zr, gPairing);
  element_init_GT(gt, gPairing);
  element_from_bytes_compressed(pk, pkey);
  element_from_hash(zr, phash, nhash);
  element_pow_zn(pk, pk, zr);
  element_to_bytes_compressed(rbuf, pk);

  element_pow_zn(pk, gG2_gen, zr);
  pairing_apply(gt, gG1_gen, pk, gPairing);
  element_to_bytes(pbuf, gt);

  element_clear(zr);
  element_clear(gt);
  element_clear(pk);
}

extern "C"
void sakai_kasahara_decrypt(unsigned char* pbuf, // pairing result in GT
			    unsigned char* rbuf, // R pt in G2
			    unsigned char* sbuf) // secret subkey in G1
{
  element_t gt, sk, rk;

  /* rk, rbuf is the R value from encryption */
  /* sk, sbuf is the secret_subkey */
  
  element_init_G1(sk, gPairing);
  element_init_G2(rk, gPairing);
  element_init_GT(gt, gPairing);
  element_from_bytes_compressed(sk, sbuf);
  element_from_bytes_compressed(rk, rbuf);
  pairing_apply(gt, sk, rk, gPairing);
  element_to_bytes(pbuf, gt);
  element_clear(sk);
  element_clear(rk);
  element_clear(gt);
}
			    
extern "C"
long sakai_kasahara_check(unsigned char* rkey, // R in G2
			  unsigned char* pkey, // public subkey in G2
			  unsigned char* phash, long nhash)
{
  element_t zr, pk1, pk2;
  long      ans;

  /* rkey, pk2 is the R value from encryption */
  /* pkey, pk1 is the public_subkey */
  /* phash is hash(ID || Tstamp || msg) */
  
  element_init_G2(pk1, gPairing);
  element_init_G2(pk2, gPairing);
  element_init_Zr(zr, gPairing);
  element_from_bytes_compressed(pk1, pkey);
  element_from_bytes_compressed(pk2, rkey);
  element_from_hash(zr, phash, nhash);
  element_pow_zn(pk1, pk1, zr);
  ans = element_cmp(pk1, pk2);
  element_clear(pk1);
  element_clear(pk2);
  element_clear(zr);
  return ans;
}

// -----------------------------------------------------------------

static long get_datum(element_t elt, unsigned char* pbuf, long buflen, bool cmpr = true)
{
  long len;

  if(cmpr)
    len = element_length_in_bytes_compressed(elt);
  else
    len = element_length_in_bytes(elt);

  if (NULL != pbuf)
    {
      if(buflen < len)
	return 0;
      if(cmpr)
	element_to_bytes_compressed(pbuf, elt);
      else
	element_to_bytes(pbuf, elt);
    }
  return len;
}

extern "C"
long get_g2(unsigned char* pbuf, long buflen)
{
  return get_datum(gG2_gen, pbuf, buflen);
}

extern "C"
long get_g1(unsigned char* pbuf, long buflen)
{
  return get_datum(gG1_gen, pbuf, buflen);
}

// ------------------------------------------------

extern "C"
long check_signature(unsigned char* psig,
		     unsigned char* phash, long nhash,
		     unsigned char *pkey)
{
  element_t ptHash, ptPKey, ptSig, pair1, pair2;
  long      tf;
  element_init_G1(ptHash, gPairing);
  element_init_G1(ptSig,  gPairing);
  element_init_G2(ptPKey, gPairing);
  element_init_GT(pair1,  gPairing);
  element_init_GT(pair2,  gPairing);
  
  element_from_bytes_compressed(ptSig, psig);
  element_from_hash(ptHash, phash, nhash);
  element_from_bytes_compressed(ptPKey, pkey);
  pairing_apply(pair1, ptSig,  gG2_gen, gPairing);
  pairing_apply(pair2, ptHash, ptPKey,  gPairing);
  tf = element_cmp(pair1, pair2);
  
  element_clear(ptHash);
  element_clear(ptPKey);
  element_clear(ptSig);
  element_clear(pair1);
  element_clear(pair2);
  
  return tf;
}

extern "C"
void mul_G1_pts(unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  // DO NOT ALLOW pt1 OR pt2 TO BE ZERO ON ENTRY!
  element_init_G1(p1, gPairing);
  element_init_G1(p2, gPairing);
  nel = element_length_in_bytes_compressed(p1);
  element_from_bytes_compressed(p1, pt1);
  element_from_bytes_compressed(p2, pt2);
  element_mul(p1, p1, p2);
  if(element_is0(p1))
    memset(pt1,0,nel);
  else
    element_to_bytes_compressed(pt1, p1);
  element_clear(p1);
  element_clear(p2);
}
  
extern "C"
void mul_G2_pts(unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2;
  long      nel;
  // DO NOT ALLOW pt1 OR pt2 TO BE ZERO ON ENTRY!
  element_init_G2(p1, gPairing);
  element_init_G2(p2, gPairing);
  nel = element_length_in_bytes_compressed(p1);
  element_from_bytes_compressed(p1, pt1);
  element_from_bytes_compressed(p2, pt2);
  element_mul(p1, p1, p2);
  if(element_is0(p1))
    memset(pt1, 0, nel);
  else
    element_to_bytes_compressed(pt1, p1);
  element_clear(p1);
  element_clear(p2);
}
  
extern "C"
void add_Zr_vals(unsigned char* zr1, unsigned char* zr2)
{
  element_t z1, z2;
  element_init_Zr(z1, gPairing);
  element_init_Zr(z2, gPairing);
  element_from_bytes(z1, zr1);
  element_from_bytes(z2, zr2);
  element_add(z1, z1, z2);
  element_to_bytes(zr1, z1);
  element_clear(z1);
  element_clear(z2);
}
  
extern "C"
void inv_Zr_val(unsigned char* zr)
{
  element_t z;
  // DO NOT ALLOW zr TO BE ZERO ON ENTRY!!
  element_init_Zr(z, gPairing);
  element_from_bytes(z, zr);
  element_invert(z, z);
  element_to_bytes(zr, z);
  element_clear(z);
}

extern "C"
void exp_G1z(unsigned char* g1, unsigned char* zr)
{
  element_t z, g;
  // DO NOT ALLOW zr TO BE ZERO ON ENTRY!!
  element_init_Zr(z, gPairing);
  element_init_G1(g, gPairing);
  element_from_bytes(z, zr);
  element_from_bytes_compressed(g, g1);
  element_pow_zn(g, g, z);
  element_to_bytes_compressed(g1, g);
  element_clear(z);
  element_clear(g);
}
  
extern "C"
void exp_G2z(unsigned char* g2, unsigned char* zr)
{
  element_t z, g;
  // DO NOT ALLOW zr TO BE ZERO ON ENTRY!!
  element_init_Zr(z, gPairing);
  element_init_G2(g, gPairing);
  element_from_bytes(z, zr);
  element_from_bytes_compressed(g, g2);
  element_pow_zn(g, g, z);
  element_to_bytes_compressed(g2, g);
  element_clear(z);
  element_clear(g);
}

extern "C"
void get_G1_from_hash(unsigned char *g1_pt, unsigned char *phash, long nhash)
{
  element_t g;

  element_init_G1(g, gPairing);
  element_from_hash(g, phash, nhash);
  element_to_bytes_compressed(g1_pt, g);
  element_clear(g);
}

extern "C"
void get_G2_from_hash(unsigned char *g2_pt, unsigned char *phash, long nhash)
{
  element_t g;

  element_init_G2(g, gPairing);
  element_from_hash(g, phash, nhash);
  element_to_bytes_compressed(g2_pt, g);
  element_clear(g);
}

extern "C"
void get_Zr_from_hash(unsigned char *zr_val, unsigned char *phash, long nhash)
{
  element_t z;

  element_init_Zr(z, gPairing);
  element_from_hash(z, phash, nhash);
  element_to_bytes(zr_val, z);
  element_clear(z);
}

// -- end of pbc_intf.cpp -- //




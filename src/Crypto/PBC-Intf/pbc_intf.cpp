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

#include <strings.h>
#include <stdlib.h>
#include <memory.h>
#include <pbc/pbc.h>

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
pairing_t pairing;
element_t g, h;
element_t public_key, secret_key;
element_t sig;
element_t temp1, temp2;
pairing_pp_t pp;

extern "C"
long init_pairing(char* param_str, long nel)
{
  long ans;
  
  if(init_flag)
    {
      pairing_pp_clear(pp);
      element_clear(temp1);
      element_clear(temp2);
      element_clear(public_key);
      element_clear(secret_key);
      element_clear(sig);
      element_clear(g);
      element_clear(h);
      pairing_clear(pairing);
    }
  ans = pairing_init_set_buf(pairing, param_str, nel);
  if(0 == ans)
    {
      element_init_G2(g, pairing);
      element_init_G2(public_key, pairing);
      element_init_G1(h, pairing);
      element_init_G1(sig, pairing);
      element_init_GT(temp1, pairing);
      element_init_GT(temp2, pairing);
      element_init_Zr(secret_key, pairing);
      element_random(g); // default random values
      element_random(h);
      element_random(secret_key);
      element_pow_zn(public_key, g, secret_key);
      init_flag = true;
    }
  return ans;
}

extern "C"
long set_g(unsigned char* pbuf)
{
  return element_from_bytes_compressed(g, pbuf);
}

extern "C"
long set_h(unsigned char* pbuf)
{
  return element_from_bytes_compressed(h, pbuf);
}

extern "C"
void set_secret_key (unsigned char* pbuf)
{
  element_from_bytes(secret_key, pbuf);
  element_pow_zn(public_key, g, secret_key);
}

extern "C"
long set_public_key (unsigned char* pbuf)
{
  return element_from_bytes_compressed(public_key, pbuf);
}

// --------------------------------------------

extern "C"
void make_key_pair(unsigned char* phash, long nhash)
{
  element_from_hash(secret_key, phash, nhash);
  element_pow_zn(public_key, g, secret_key);
}

extern "C"
void sign_hash(unsigned char* phash, long nhash)
{
  element_from_hash(h, phash, nhash);
  element_pow_zn(sig, h, secret_key);
}

static long get_datum(element_t elt, unsigned char* pbuf, long *plen, bool cmpr = true)
{
  long len;

  if(cmpr)
    len = element_length_in_bytes_compressed(elt);
  else
    len = element_length_in_bytes(elt);

  if (NULL == pbuf)
    {
      if(NULL != plen)
	*plen = len;
      return len;
    }
  else
    {
      if(NULL == plen)
	return 0;
      if(*plen < len)
	return 0;
      if(cmpr)
	element_to_bytes_compressed(pbuf, elt);
      else
	element_to_bytes(pbuf, elt);
      return (*plen = len);
     }
}

extern "C"
long get_secret_key(unsigned char* pbuf, long *plen)
{
  return get_datum(secret_key, pbuf, plen, false);
}

extern "C"
long get_public_key(unsigned char* pbuf, long *plen)
{
  return get_datum(public_key, pbuf, plen);
}

extern "C"
long get_g(unsigned char* pbuf, long *plen)
{
  return get_datum(g, pbuf, plen);
}

extern "C"
long get_h(unsigned char* pbuf, long *plen)
{
  return get_datum(h, pbuf, plen);
}

extern "C"
long get_signature(unsigned char* pbuf, long *plen)
{
  return get_datum(sig, pbuf, plen);
}

// ------------------------------------------------

extern "C"
long check_signature(unsigned char* psig, unsigned char* phash, long nhash, unsigned char *pkey)
{
  element_from_bytes_compressed(sig, psig);
  element_from_hash(h, phash, nhash);
  element_from_bytes_compressed(public_key, pkey);
  pairing_apply(temp1, sig, g, pairing);
  pairing_apply(temp2, h, public_key, pairing);
  return element_cmp(temp1, temp2);
}

extern "C"
void mul_G1_pts(unsigned char* pt_sum, unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2, psum;
  element_init_G1(p1, pairing);
  element_init_G1(p2, pairing);
  element_init_G1(psum, pairing);
  element_from_bytes_compressed(p1, pt1);
  element_from_bytes_compressed(p2, pt2);
  element_mul(psum, p1, p2);
  element_to_bytes_compressed(pt_sum, psum);
  element_clear(p1);
  element_clear(p2);
  element_clear(psum);
}
  
extern "C"
void mul_G2_pts(unsigned char* pt_sum, unsigned char* pt1, unsigned char* pt2)
{
  element_t p1, p2, psum;
  element_init_G2(p1, pairing);
  element_init_G2(p2, pairing);
  element_init_G2(psum, pairing);
  element_from_bytes_compressed(p1, pt1);
  element_from_bytes_compressed(p2, pt2);
  element_mul(psum, p1, p2);
  element_to_bytes_compressed(pt_sum, psum);
  element_clear(p1);
  element_clear(p2);
  element_clear(psum);
}
  
extern "C"
void add_Zr_vals(unsigned char* zr_sum, unsigned char* zr1, unsigned char* zr2)
{
  element_t z1, z2, zsum;
  element_init_Zr(z1, pairing);
  element_init_Zr(z2, pairing);
  element_init_Zr(zsum, pairing);
  element_from_bytes(z1, zr1);
  element_from_bytes(z2, zr2);
  element_add(zsum, z1, z2);
  element_to_bytes(zr_sum, zsum);
  element_clear(z1);
  element_clear(z2);
  element_clear(zsum);
}
  
// -- end of pbc_intf.cpp -- //




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
void init_pairing(char* param_str, long nel)
{
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
  pairing_init_set_buf(pairing, param_str, nel);
  element_init_G2(g, pairing);
  element_init_G2(public_key, pairing);
  element_init_G1(h, pairing);
  element_init_G1(sig, pairing);
  element_init_GT(temp1, pairing);
  element_init_GT(temp2, pairing);
  element_init_Zr(secret_key, pairing);
  element_random(g); // default values
  element_random(h);
  element_random(secret_key);
  init_flag = true;
}

extern "C"
long set_g(unsigned char* pbuf)
{
  return element_from_bytes(g, pbuf);
}

extern "C"
long set_h(unsigned char* pbuf)
{
  return element_from_bytes(h, pbuf);
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
  return element_from_bytes(public_key, pbuf);
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

static long get_datum(element_t elt, unsigned char* pbuf, long *plen)
{
  long len = element_length_in_bytes(elt);
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
      element_to_bytes(pbuf, elt);
      return (*plen = len);
     }
}

extern "C"
long get_secret_key(unsigned char* pbuf, long *plen)
{
  return get_datum(secret_key, pbuf, plen);
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
void get_signature(unsigned char* pbuf, long *plen)
{
  get_datum(sig, pbuf, plen);
}

// ------------------------------------------------

extern "C"
long check_signature(unsigned char* psig, unsigned char* phash, long nhash, unsigned char *pkey)
{
  element_from_bytes(sig, psig);
  element_from_hash(h, phash, nhash);
  element_from_bytes(public_key, pkey);
  pairing_apply(temp1, sig, g, pairing);
  pairing_apply(temp2, h, public_key, pairing);
  return element_cmp(temp1, temp2);
}

// -- end of pbc_intf.cpp -- //




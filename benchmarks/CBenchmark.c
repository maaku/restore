/* Copyright (c) 2006-2019 The Binary Developers.
 * Copyright (c) 2019 The Restore Developers.
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0.  If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#include "CBenchmark.h"

void bytewrite(unsigned char *a, int bytes) {
  unsigned char n = 0;
  int i = 0;
  int iterations = bytes;
  while (i < iterations) {
    a[i++] = n++;
  }
}

unsigned char byteread(unsigned char *a, int bytes) {
  unsigned char n = 0;
  int i = 0;
  int iterations = bytes;
  while (i < iterations) {
    n += a[i++];
  }
  return n;
}

void wordwrite(unsigned long *a, int bytes) {
  unsigned long n = 0;
  int i = 0;
  int iterations = bytes / sizeof(unsigned long) ;
  while (i < iterations) {
    a[i++] = n++;
  }
}

unsigned int wordread(unsigned long *a, int bytes) {
  unsigned long n = 0;
  int i = 0;
  int iterations = bytes / sizeof(unsigned long);
  while (i < iterations) {
    n += a[i++];
  }
  return n;
}

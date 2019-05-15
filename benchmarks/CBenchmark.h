/* Copyright (c) 2006-2019 The Binary Developers.
 * Copyright (c) 2019 The Restore Developers.
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0.  If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

void bytewrite(unsigned char *a, int bytes);
unsigned char byteread(unsigned char *a, int bytes);
void wordwrite(unsigned long *a, int bytes);
unsigned int wordread(unsigned long *a, int bytes);

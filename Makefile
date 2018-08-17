#-
# Copyright (c) 2018 Alexandre Joannou
# Copyright (c) 2018 Matthew Naylor
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

BSC = bsc

BLUEUTILSDIR = ./BlueBasics
BSVPATH = +:$(BLUEUTILSDIR)

BSCFLAGS = -p $(BSVPATH) -check-assert
BSCFLAGS += -show-schedule -sched-dot
BSCFLAGS += -show-range-conflict
BSCFLAGS += -bdir bdir -simdir simdir -info-dir info-dir

all: $(patsubst Example%.bsv, example%, $(wildcard Example*.bsv))

example%: Example%.bsv Recipe.bsv
	mkdir -p bdir simdir info-dir
	$(BSC) -cpp $(BSCFLAGS) -u -sim -g top $<
	$(BSC) $(BSCFLAGS) -sim -e top -o $@
	dot -Tsvg info-dir/top_exec.dot > info-dir/top_exec.svg
	dot -Tsvg info-dir/top_urgency.dot > info-dir/top_urgency.svg
	dot -Tsvg info-dir/top_combined.dot > info-dir/top_combined.svg
	dot -Tsvg info-dir/top_conflict.dot > info-dir/top_conflict.svg

.PHONY: clean
clean:
	rm -fr *example*
	rm -fr bdir simdir info-dir
	ls Recipe.* | grep -v Recipe.bsv | xargs rm -f

// Copyright 2019 kai
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import * as llvm from "llvm-node";

export function test_gen() {
  const context = new llvm.LLVMContext();
  const module = new llvm.Module("test", context);
  const intType = llvm.Type.getInt32Ty(context);
  const initializer = llvm.ConstantInt.get(context, 0);
  const globalVariable = new llvm.GlobalVariable(module, intType, true, llvm.LinkageTypes.InternalLinkage, initializer);
  const ll = module.print(); // prints IR
  console.log(ll);
  llvm.writeBitcodeToFile(module, "biteCodeFileName.bc"); // Writes file to disk
}

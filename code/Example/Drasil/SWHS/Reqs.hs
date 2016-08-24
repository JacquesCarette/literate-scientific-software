module Example.Drasil.SWHS.Reqs where

import Language.Drasil
import Example.Drasil.SWHS.Modules

reqs :: [ReqChunk]
reqs = [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11]

r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11 :: ReqChunk

r1 = ReqChunk (CC "" $ S "") [mod_hw, mod_inputf, mod_inputp, mod_ctrl]
r2 = ReqChunk (CC "" $ S "") [mod_inputf, mod_inputp]
r3 = ReqChunk (CC "" $ S "") [mod_inputv]
r4 = ReqChunk (CC "" $ S "") [mod_outputf, mod_ctrl]
r5 = ReqChunk (CC "" $ S "") [mod_outputf, mod_temp, mod_ctrl, mod_seq, mod_ode,
  mod_plot]
r6 = ReqChunk (CC "" $ S "") [mod_outputf, mod_temp, mod_ctrl, mod_seq, mod_ode,
  mod_plot]
r7 = ReqChunk (CC "" $ S "") [mod_outputf, mod_ener, mod_ctrl, mod_seq, mod_plot]
r8 = ReqChunk (CC "" $ S "") [mod_outputf, mod_ener, mod_ctrl, mod_seq, mod_plot]
r9 = ReqChunk (CC "" $ S "") [mod_outputv]
r10 = ReqChunk (CC "" $ S "") [mod_outputf, mod_temp, mod_ctrl]
r11 = ReqChunk (CC "" $ S "") [mod_outputf, mod_temp, mod_ener, mod_ctrl]

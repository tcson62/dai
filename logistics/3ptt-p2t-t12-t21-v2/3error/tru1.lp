agent(2, constant("tru1")).
neighbor(4, constant("apn2")).
neighbor(1, constant("apn1")).

domain("domain.lp").
problem("prob2_tru1.lp").

occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),1).
occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),2).
occurs(constant("tru1"),("unload-truck",constant("obj11"),constant("tru1"),constant("apt1")),3).
occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),5).
occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),6).
occurs(constant("tru1"),("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),7).
occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),8).
occurs(constant("tru1"),("unload-truck",constant("obj23"),constant("tru1"),constant("pos1")),9).

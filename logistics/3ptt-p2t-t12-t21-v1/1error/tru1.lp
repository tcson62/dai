agent(2, constant("tru1")).
neighbor(1, constant("apn1")). 

domain("../domain.lp").
problem("prob1_tru1.lp").


occurs(constant("tru1"),("load-truck",constant("obj13"),constant("tru1"),constant("pos1")),2).
occurs(constant("tru1"),("load-truck",constant("obj11"),constant("tru1"),constant("pos1")),3).
occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),4).
occurs(constant("tru1"),("unload-truck",constant("obj11"),constant("tru1"),constant("apt1")),5).
occurs(constant("tru1"),("unload-truck",constant("obj13"),constant("tru1"),constant("apt1")),6).
occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),7).
occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("pos1"),constant("apt1"),constant("cit1")),8).
occurs(constant("tru1"),("load-truck",constant("obj21"),constant("tru1"),constant("apt1")),9).
occurs(constant("tru1"),("load-truck",constant("obj23"),constant("tru1"),constant("apt1")),10).
occurs(constant("tru1"),("drive-truck",constant("tru1"),constant("apt1"),constant("pos1"),constant("cit1")),11).
occurs(constant("tru1"),("unload-truck",constant("obj21"),constant("tru1"),constant("pos1")),12).
occurs(constant("tru1"),("unload-truck",constant("obj23"),constant("tru1"),constant("pos1")),13).

agent(3, constant("tru2")).
neighbor(4, constant("apn2")).
neighbor(1, constant("apn1")).

domain("domain.lp").
problem("prob2_tru2.lp").

occurs(constant("tru2"),("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),1).
occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),2).
occurs(constant("tru2"),("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),3).
occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),4).
occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),6).
occurs(constant("tru2"),("load-truck",constant("obj11"),constant("tru2"),constant("apt2")),7).
occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),8).
occurs(constant("tru2"),("unload-truck",constant("obj11"),constant("tru2"),constant("pos2")),9).

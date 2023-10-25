agent(3, constant("tru2")).
neighbor(1, constant("apn1")). 

domain("../domain.lp").
problem("prob1_tru2.lp").

occurs(constant("tru2"),("load-truck",constant("obj23"),constant("tru2"),constant("pos2")),1).
occurs(constant("tru2"),("load-truck",constant("obj21"),constant("tru2"),constant("pos2")),2).
occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("pos2"),constant("apt2"),constant("cit2")),3).
occurs(constant("tru2"),("unload-truck",constant("obj23"),constant("tru2"),constant("apt2")),4).
occurs(constant("tru2"),("unload-truck",constant("obj21"),constant("tru2"),constant("apt2")),5).
occurs(constant("tru2"),("drive-truck",constant("tru2"),constant("apt2"),constant("pos2"),constant("cit2")),6).
occurs(constant("tru2"),("load-truck",constant("obj22"),constant("tru2"),constant("pos2")),12).

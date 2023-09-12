agent(4, constant("apn2")).
neighbor(2, constant("tru1")).
neighbor(3, constant("tru2")).
neighbor(1, constant("apn1")).

domain("domain.lp").
problem("prob2_apn2.lp").

occurs(constant("apn2"),("fly-airplane",constant("apn2"),constant("apt1"),constant("apt2")),1).
occurs(constant("apn2"),("load-airplane",constant("obj23"),constant("apn2"),constant("apt2")),4).
occurs(constant("apn2"),("fly-airplane",constant("apn2"),constant("apt2"),constant("apt1")),5).
occurs(constant("apn2"),("unload-airplane",constant("obj23"),constant("apn2"),constant("apt1")),6).
occurs(constant("apn2"),("fly-airplane",constant("apn2"),constant("apt1"),constant("apt2")),7).
occurs(constant("apn2"),("fly-airplane",constant("apn2"),constant("apt2"),constant("apt1")),8).
occurs(constant("apn2"),("fly-airplane",constant("apn2"),constant("apt1"),constant("apt2")),9).

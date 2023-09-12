agent(1, constant("apn1")).
neighbor(2, constant("tru1")).
neighbor(3, constant("tru2")). 
neighbor(4, constant("apn2")).

domain("domain.lp").
problem("prob2_apn1.lp").


occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),1).
occurs(constant("apn1"),("load-airplane",constant("obj11"),constant("apn1"),constant("apt1")),4).
occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),5).
occurs(constant("apn1"),("unload-airplane",constant("obj11"),constant("apn1"),constant("apt2")),6).
occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),8).
occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),9).

agent(1, constant("apn1")).
neighbor(2, constant("tru1")).
neighbor(3, constant("tru2")). 

domain("../../domain.lp").
problem("prob1_apn1.lp").


occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),1).
occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),2).
occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),3).
occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt1"),constant("apt2")),4).
occurs(constant("apn1"),("load-airplane",constant("obj23"),constant("apn1"),constant("apt2")),5).
occurs(constant("apn1"),("load-airplane",constant("obj21"),constant("apn1"),constant("apt2")),6).
occurs(constant("apn1"),("fly-airplane",constant("apn1"),constant("apt2"),constant("apt1")),7).
occurs(constant("apn1"),("unload-airplane",constant("obj21"),constant("apn1"),constant("apt1")),8).
occurs(constant("apn1"),("unload-airplane",constant("obj23"),constant("apn1"),constant("apt1")),9).
occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),10).
occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),11).
occurs(constant("apn1"),("load-airplane",constant("obj13"),constant("apn1"),constant("apt1")),12).
occurs(constant("apn1"),("unload-airplane",constant("obj13"),constant("apn1"),constant("apt1")),13).

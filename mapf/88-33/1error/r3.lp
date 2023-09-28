agent(3, 3).
neighbor(2, 2).
neighbor(1, 1). 

domain("../../mapf_domain.lp").
problem("prob1_3.lp").

occurs(3,("up",3,6,0,1),4).
occurs(3,("left",3,6,1,5),5).
occurs(3,("up",3,5,1,2),6).
occurs(3,("up",3,5,2,3),7).
occurs(3,("up",3,5,3,4),8).
occurs(3,("left",3,5,4,4),10).
occurs(3,("left",3,4,4,3),11).
occurs(3,("down",3,3,4,3),12).
occurs(3,("down",3,3,3,2),13).
occurs(3,("down",3,3,2,1),14).
occurs(3,("left",3,3,1,2),15).

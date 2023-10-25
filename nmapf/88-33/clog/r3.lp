agent(3, 3).
neighbor(2, 2).
neighbor(1, 1). 

domain("../../mapf_domain.lp").
problem("prob1_3.lp").

occurs(3,("up",3,6,0,1),1).
occurs(3,("up",3,6,1,2),2).
occurs(3,("left",3,6,2,5),3).
occurs(3,("up",3,5,2,3),4).
occurs(3,("up",3,5,3,4),5).
occurs(3,("left",3,5,4,4),6).
occurs(3,("left",3,4,4,3),7).
occurs(3,("left",3,3,4,2),8).
occurs(3,("left",3,2,4,1),9).
occurs(3,("stay",3,1,4),10).
occurs(3,("right",3,1,4,2),11).
occurs(3,("down",3,2,4,3),12).
occurs(3,("down",3,2,3,2),13).
occurs(3,("down",3,2,2,1),14).
occurs(3,("up",3,2,1,2),15).
occurs(3,("stay",3,2,2),16).
occurs(3,("down",3,2,2,1),17).
occurs(3,("right",3,2,1,3),18).

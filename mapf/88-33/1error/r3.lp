agent(3, 3).
neighbor(2, 2).
neighbor(1, 1). 

domain("../../mapf_domain.lp").
problem("prob1_3.lp").

occurs(3,("stay",3,6,0),1).
occurs(3,("up",3,6,0,1),2).
occurs(3,("left",3,6,1,5),3).
occurs(3,("stay",3,5,1),4).
occurs(3,("up",3,5,1,2),5).
occurs(3,("up",3,5,2,3),6).
occurs(3,("up",3,5,3,4),7).
occurs(3,("down",3,5,4,3),8).
occurs(3,("stay",3,5,3),9).
occurs(3,("up",3,5,3,4),10).
occurs(3,("left",3,5,4,4),11).
occurs(3,("left",3,4,4,3),12).
occurs(3,("down",3,3,4,3),13).
occurs(3,("stay",3,3,3),14).
occurs(3,("down",3,3,3,2),15).
occurs(3,("down",3,3,2,1),16).
occurs(3,("left",3,3,1,2),17).
occurs(3,("right",3,2,1,3),18).

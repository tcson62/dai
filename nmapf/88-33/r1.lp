agent(1, 1).
neighbor(2, 2).
neighbor(3, 3). 

domain("mapf_domain.lp").
problem("prob1_1.lp").

occurs(1,("down",1,7,7,6),1).
occurs(1,("left",1,7,6,6),2).
occurs(1,("down",1,6,6,5),3).
occurs(1,("down",1,6,5,4),4).
occurs(1,("stay",1,6,4),5).
occurs(1,("up",1,6,4,5),6).
occurs(1,("down",1,6,5,4),7).
occurs(1,("down",1,6,4,3),8).
occurs(1,("stay",1,6,3),9).
occurs(1,("up",1,6,3,4),10).
occurs(1,("left",1,6,4,5),11).
occurs(1,("left",1,5,4,4),12).
occurs(1,("left",1,4,4,3),13).
occurs(1,("left",1,3,4,2),14).
occurs(1,("down",1,2,4,3),15).
occurs(1,("left",1,2,3,1),16).
occurs(1,("left",1,1,3,0),17).
occurs(1,("stay",1,0,3),18).

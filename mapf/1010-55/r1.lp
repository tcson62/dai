agent(1, 1).
neighbor(2, 2).
neighbor(3, 3). 
neighbor(4, 4).
neighbor(5, 5). 

domain("mapf_domain.lp").
problem("prob2_1.lp").

occurs(1,("left",1,7,6,6),1).
occurs(1,("down",1,6,6,5),2).
occurs(1,("left",1,6,5,5),3).
occurs(1,("left",1,5,5,4),4).
occurs(1,("up",1,4,5,6),5).
occurs(1,("left",1,4,6,3),6).
occurs(1,("stay",1,3,6),7).
occurs(1,("down",1,3,6,5),8).
occurs(1,("stay",1,3,5),9).
occurs(1,("down",1,3,5,4),10).
occurs(1,("down",1,3,4,3),11).
occurs(1,("down",1,3,3,2),12).
occurs(1,("down",1,3,2,1),13).
occurs(1,("left",1,3,1,2),14).
occurs(1,("stay",1,2,1),15).

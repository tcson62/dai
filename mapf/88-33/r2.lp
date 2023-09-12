agent(2, 2).
neighbor(1, 1).
neighbor(3, 3). 

domain("mapf_domain.lp").
problem("prob1_2.lp").

occurs(2,("up",2,0,0,1),1).
occurs(2,("up",2,0,1,2),2).
occurs(2,("up",2,0,2,3),3).
occurs(2,("right",2,0,3,1),4).
occurs(2,("right",2,1,3,2),5).
occurs(2,("right",2,2,3,3),6).
occurs(2,("up",2,3,3,4),7).
occurs(2,("right",2,3,4,4),8).
occurs(2,("right",2,4,4,5),9).
occurs(2,("up",2,5,4,5),10).
occurs(2,("right",2,5,5,6),11).
occurs(2,("down",2,6,5,4),12).
occurs(2,("down",2,6,4,3),13).
occurs(2,("down",2,6,3,2),14).
occurs(2,("down",2,6,2,1),15).
occurs(2,("down",2,6,1,0),16).
occurs(2,("right",2,6,0,7),17).
occurs(2,("stay",2,7,0),18).

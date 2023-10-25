agent(2, 2).
neighbor(5, 5).
neighbor(3, 3). 
neighbor(4, 4).
neighbor(1, 1). 

domain("../../mapf_domain.lp").
problem("prob2_2.lp").

occurs(2,("left",2,3,2,2),1).
occurs(2,("up",2,2,2,3),2).
occurs(2,("up",2,2,3,4),3).
occurs(2,("stay",2,2,4),4).
occurs(2,("up",2,2,4,5),5).
occurs(2,("right",2,2,5,3),6).
occurs(2,("right",2,3,5,4),7).
occurs(2,("right",2,4,5,5),8).
occurs(2,("right",2,5,5,6),9).
occurs(2,("down",2,6,5,4),10).
occurs(2,("down",2,6,4,3),11).
occurs(2,("down",2,6,3,2),12).
occurs(2,("down",2,6,2,1),13).
occurs(2,("right",2,6,1,7),14).
occurs(2,("left",2,7,1,6),15).

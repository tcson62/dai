agent(4, 4).
neighbor(2, 2).
neighbor(3, 3). 
neighbor(5, 5).
neighbor(1, 1). 

domain("mapf_domain.lp").
problem("prob2_4.lp").

occurs(4,("right",4,0,5,1),1).
occurs(4,("stay",4,1,5),2).
occurs(4,("right",4,1,5,2),3).
occurs(4,("right",4,2,5,3),4).
occurs(4,("right",4,3,5,4),5).
occurs(4,("right",4,4,5,5),6).
occurs(4,("right",4,5,5,6),7).
occurs(4,("down",4,6,5,4),8).
occurs(4,("down",4,6,4,3),9).
occurs(4,("down",4,6,3,2),10).
occurs(4,("down",4,6,2,1),11).
occurs(4,("down",4,6,1,0),12).
occurs(4,("right",4,6,0,7),13).
occurs(4,("right",4,7,0,8),14).
occurs(4,("stay",4,8,0),15).

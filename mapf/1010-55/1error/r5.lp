agent(5, 5).
neighbor(2, 2).
neighbor(3, 3). 
neighbor(4, 4).
neighbor(1, 1). 

domain("../../mapf_domain.lp").
problem("prob2_5.lp").

occurs(5,("right",5,2,4,3),1).
occurs(5,("stay",5,3,4),2).
occurs(5,("right",5,3,4,4),3).
occurs(5,("down",5,4,4,3),4).
occurs(5,("stay",5,4,3),5).
occurs(5,("stay",5,4,3),6).
occurs(5,("up",5,4,3,4),7).
occurs(5,("down",5,4,4,3),8).
occurs(5,("up",5,4,3,4),9).
occurs(5,("up",5,4,4,5),10).
occurs(5,("right",5,4,5,5),11).
occurs(5,("right",5,5,5,6),12).
occurs(5,("down",5,6,5,4),13).
occurs(5,("up",5,6,4,5),14).
occurs(5,("left",5,6,5,5),15).

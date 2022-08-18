import java.awt.Point;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import TSim.*;

public class Lab1 {
	
	public TSimInterface tsi = TSimInterface.getInstance();
	// Declare HashMap to get the conditions depending on the sensor of 
	// the train.
	HashMap<Point,Integer> sensorsMap = new HashMap<>();
	public enum Way {
			North, South
	}
	
	// Declare the sensors that were put on the map 
	public Point switch_Upper_Right = new Point(17,7);
	public Point switch_Middle_Right = new Point(15,9);
	public Point switch_Middle_Left = new Point(4,9);
	public Point switch_Bottom_Left = new Point(3,11);
	
	// Declare the Semaphores to protect the critical sections
	public Semaphore northA_Road = new Semaphore(1,true);
	public Semaphore northB_Road = new Semaphore(1,true);
	public Semaphore middleA_Road = new Semaphore(1,true);
	public Semaphore middleB_Road = new Semaphore(1,true);
	public Semaphore southA_Road = new Semaphore(1);
	public Semaphore southB_Road = new Semaphore(1,true);
	public Semaphore north_Intersection = new Semaphore(1,true);
	public Semaphore east_Road = new Semaphore(1,true);
	public Semaphore west_Road = new Semaphore(1,true);
	
	public Lab1(int speed1, int speed2) throws InterruptedException {
		// Three Sensors for Upper Switch 
		sensorsMap.put(new Point(15,7), 0);
		sensorsMap.put(new Point(18,7), 1);
		sensorsMap.put(new Point(17,8), 2);
		
		// Three Sensors for Right Middle Switch 
		sensorsMap.put(new Point(14,9), 3);
		sensorsMap.put(new Point(16,9), 4);
		sensorsMap.put(new Point(14,10), 5);
		
		// Three Sensors for Left Middle Switch 
		sensorsMap.put(new Point(3,9), 6);
		sensorsMap.put(new Point(6,9), 7);
		sensorsMap.put(new Point(5,10), 8);
		
		// Three Sensors for Left Downward Switch 
		sensorsMap.put(new Point(2,11), 9);
		sensorsMap.put(new Point(5,11), 10);
		sensorsMap.put(new Point(3,12), 11);
		
		// Sensor at Top North Station 
		sensorsMap.put(new Point(15,3), 12);
		// Sensor at Bottom North Station 
		sensorsMap.put(new Point(15,5), 13);
		
		// Sensor at Top South Station
		sensorsMap.put(new Point(15,11), 14);
		// Sensor at Bottom South Station
		sensorsMap.put(new Point(15,13), 15);
		
		//Sensors at Intersection
		sensorsMap.put(new Point(6,6), 16);
		sensorsMap.put(new Point(10,7), 17);
		sensorsMap.put(new Point(8,5), 18);
		sensorsMap.put(new Point(10,8), 19);
		
		
		Runnable train1 = new Train(speed1,1);
	    Runnable train2 = new Train(speed2,2);
	    
	    // Get the two threads running
	    Thread thread1 = new Thread(train1);
	    Thread thread2 = new Thread(train2);
	    thread1.start();
	    thread2.start();
		 
	  }
  
	public class Train implements Runnable {
		// Variable of the speed of the train
		public int speed;
		
		// Variable to know the id of the train
		public int train_id;
		
		// Variable to know the direction of the train
		// if it's going to the South or North
		public Way way;
	  
		public Train(int speed, int train_id) throws InterruptedException {
			this.speed = speed;
			this.train_id = train_id;
			settingSpeeds();
		}
		
		// The method assigns the speed of the train depending on the argument given by the user
		public void settingSpeeds() throws InterruptedException {
			try {
				
			    tsi.setSpeed(train_id,speed);
			    if(train_id == 1) {
			    	way = Way.South;
			    	northA_Road.acquire();
			    } else {
			    	way = Way.North;
			    	southA_Road.acquire();
			    	
			    }
			}
			catch (CommandException e) {
			    e.printStackTrace();    // or only e.getMessage() for the error
			    System.exit(1);
			}
		}
		
		// Override method to keep running the trains non-stop
		// With the class SensorEvent, we can get the specific case of what
		// should the train do depending on the coordinates of each sensor
		@Override
		public void run() {
			SensorEvent activeSensor;
		  
			while(true) {
				try {
					activeSensor = tsi.getSensor(train_id);
					Point coordinate = new Point(activeSensor.getXpos(), activeSensor.getYpos());
					if(sensorsMap.containsKey(coordinate)) {
						changeSwitch(sensorsMap.get(coordinate), coordinate, activeSensor.getStatus());
					}
				} catch (CommandException | InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		
		// The most important method, which depending on the case(sensor) the train
		// just passed through, will know what to do
		public void changeSwitch(int decision, Point coordinate, int currentSensor) throws CommandException, InterruptedException {
			switch(decision) {
			// Case 0-2 Upper Switches
			
			// The logic for this case is try to go around a corner
			// If the semaphore (critical section) is taken by the other train
			// it will stop for 3 seconds to let the train leave the corner
			// then acquire the semaphore to make sure the other train does not go around the same corner
			// and release the critical section it just traveled so the other train can take it.
			// It also changes the switch to make sure the train can pass through it.
			case 0: 
				if(way == Way.South && currentSensor == SensorEvent.ACTIVE) {
					if(!east_Road.tryAcquire()){
						tsi.setSpeed(train_id, 0);
						TimeUnit.SECONDS.sleep(3);
						tsi.setSpeed(train_id, speed);
						northA_Road.release();
						east_Road.tryAcquire();
						tsi.setSwitch(switch_Upper_Right.x, switch_Upper_Right.y, TSimInterface.SWITCH_RIGHT);
					}else {
						tsi.setSwitch(switch_Upper_Right.x, switch_Upper_Right.y, TSimInterface.SWITCH_RIGHT);
						northA_Road.release();
					}
				} 
				break;
			// The logic for this case is to release the semaphore that protects the corner,
			// and also to know which route should it choose, the upper or bottom one, depending if a train is passing through it
			case 1:
				if(way == Way.North) {
					if(currentSensor == SensorEvent.ACTIVE) {
						if(northA_Road.tryAcquire()) {
							tsi.setSwitch(switch_Upper_Right.x, switch_Upper_Right.y, TSimInterface.SWITCH_RIGHT);
						} else {
							tsi.setSwitch(switch_Upper_Right.x, switch_Upper_Right.y, TSimInterface.SWITCH_LEFT);
							northB_Road.acquire();
							tsi.setSwitch(switch_Upper_Right.x, switch_Upper_Right.y, TSimInterface.SWITCH_LEFT);
						}
					} else if(currentSensor == SensorEvent.INACTIVE) {
						east_Road.release();
					}
				}
				break;
				
			// The logic for this case is try to go around a corner
			// If the semaphore (critical section) is taken by the other train
			// it will stop for 3 seconds to let the train leave the corner
			// then acquire the semaphore to make sure the other train does not go around the same corner
			// and release the critical section it just traveled so the other train can take it.
			// It also changes the switch to make sure the train can pass through it.
			case 2:
				if(way == Way.South && currentSensor == SensorEvent.ACTIVE) {
					if(!east_Road.tryAcquire()){
						tsi.setSpeed(train_id, 0);
						TimeUnit.SECONDS.sleep(3);
						tsi.setSpeed(train_id, speed);
						northB_Road.release();
						east_Road.tryAcquire();
						tsi.setSwitch(switch_Upper_Right.x, switch_Upper_Right.y, TSimInterface.SWITCH_LEFT);
					}else {
						tsi.setSwitch(switch_Upper_Right.x, switch_Upper_Right.y, TSimInterface.SWITCH_LEFT);
						northB_Road.release();
					}
				}
				break;
				
			// Case 3-5 Right Middle Switches
				
			// The logic for this case is try to go around a corner
			// If the semaphore (critical section) is taken by the other train
			// it will stop for 3 seconds to let the train leave the corner
			// then acquire the semaphore to make sure the other train does not go around the same corner
			// and release the critical section it just traveled so the other train can take it.
			// It also changes the switch to make sure the train can pass through it.
			case 3: 
				if(way == Way.North) {
					tsi.setSwitch(switch_Middle_Right.x, switch_Middle_Right.y, TSimInterface.SWITCH_RIGHT);
					middleA_Road.release();
					if(!east_Road.tryAcquire() && currentSensor == SensorEvent.ACTIVE){
						tsi.setSpeed(train_id, 0);
						TimeUnit.SECONDS.sleep(2);
						tsi.setSpeed(train_id, speed);
						east_Road.tryAcquire();
						tsi.setSwitch(switch_Middle_Right.x, switch_Middle_Right.y, TSimInterface.SWITCH_RIGHT);
					}
				}
				break;
				
			// The logic for this case is to release the semaphore that protects the corner,
			// and also to know which route should it choose, the upper or bottom one, depending if a train is passing through it
			case 4:			
				if(way == Way.South) {
					if(currentSensor == SensorEvent.ACTIVE) {
						east_Road.release();
						if(middleA_Road.tryAcquire()) {
							tsi.setSwitch(switch_Middle_Right.x, switch_Middle_Right.y, TSimInterface.SWITCH_RIGHT);
						} else {
							tsi.setSwitch(switch_Middle_Right.x, switch_Middle_Right.y, TSimInterface.SWITCH_LEFT);
							middleB_Road.acquire();
						}
					}
				}
				break;
				
			// The logic for this case is try to go around a corner
			// If the semaphore (critical section) is taken by the other train
			// it will stop for 3 seconds to let the train leave the corner
			// then acquire the semaphore to make sure the other train does not go around the same corner
			// and release the critical section it just traveled so the other train can take it.
			// It also changes the switch to make sure the train can pass through it.	
			case 5:
				if(way == Way.North) {
					tsi.setSwitch(switch_Middle_Right.x, switch_Middle_Right.y, TSimInterface.SWITCH_LEFT);
					middleB_Road.release();
					if(!east_Road.tryAcquire() && currentSensor == SensorEvent.ACTIVE){
						tsi.setSpeed(train_id, 0);
						TimeUnit.SECONDS.sleep(2);
						tsi.setSpeed(train_id, speed);
						tsi.setSwitch(switch_Middle_Right.x, switch_Middle_Right.y, TSimInterface.SWITCH_LEFT);
						east_Road.tryAcquire();
					}
				}
				break;
				
			// Case 6-8 Left Middle Switches
			
			// The logic for this case is to release the semaphore that protects the corner,
			// and also to know which route should it choose, the upper or bottom one, depending if a train is passing through it
			case 6: 
				if(way == Way.North) {
					if(currentSensor == SensorEvent.ACTIVE) {
						west_Road.release();
						if(middleB_Road.tryAcquire()) {
							tsi.setSwitch(switch_Middle_Left.x, switch_Middle_Left.y, TSimInterface.SWITCH_RIGHT);
						} else {
							tsi.setSwitch(switch_Middle_Left.x, switch_Middle_Left.y, TSimInterface.SWITCH_LEFT);
							middleA_Road.acquire();
						}
					}
				}
				break;
				
			// The logic for this case is try to go around a corner
			// If the semaphore (critical section) is taken by the other train
			// it will stop for 3 seconds to let the train leave the corner
			// then acquire the semaphore to make sure the other train does not go around the same corner
			// and release the critical section it just traveled so the other train can take it.
			// It also changes the switch to make sure the train can pass through it.
			case 7:
				if(way == Way.South && currentSensor == SensorEvent.ACTIVE) {
					tsi.setSwitch(switch_Middle_Left.x, switch_Middle_Left.y, TSimInterface.SWITCH_LEFT);
					if(!west_Road.tryAcquire()){
						tsi.setSpeed(train_id, 0);
						TimeUnit.SECONDS.sleep(3);
						tsi.setSpeed(train_id, speed);
						middleA_Road.release();
						west_Road.tryAcquire();
						tsi.setSwitch(switch_Middle_Left.x, switch_Middle_Left.y, TSimInterface.SWITCH_LEFT);
					}else {
						middleA_Road.release();
					}
				}
				break;
				
			// The logic for this case is try to go around a corner
			// If the semaphore (critical section) is taken by the other train
			// it will stop for 3 seconds to let the train leave the corner
			// then acquire the semaphore to make sure the other train does not go around the same corner
			// and release the critical section it just traveled so the other train can take it.
			// It also changes the switch to make sure the train can pass through it.	
			case 8:
				if(way == Way.South && currentSensor == SensorEvent.ACTIVE) {
					tsi.setSwitch(switch_Middle_Left.x, switch_Middle_Left.y, TSimInterface.SWITCH_RIGHT);
					if(!west_Road.tryAcquire()){
						tsi.setSpeed(train_id, 0);
						TimeUnit.SECONDS.sleep(3);
						tsi.setSpeed(train_id, speed);
						middleB_Road.release();
						west_Road.tryAcquire();
						tsi.setSwitch(switch_Middle_Left.x, switch_Middle_Left.y, TSimInterface.SWITCH_RIGHT);
					}else {
						middleB_Road.release();
					}
				}
				break;	
			
		    // Case 9-11 Bottom Switches
			
			// The logic for this case is to release the semaphore that protects the corner,
			// and also to know which route should it choose, the upper or bottom one, depending if a train is passing through it
			case 9: 
				if(way == Way.South) {
					if(currentSensor == SensorEvent.ACTIVE) {
						west_Road.release();
						if(southA_Road.tryAcquire()) {
							tsi.setSwitch(switch_Bottom_Left.x, switch_Bottom_Left.y, TSimInterface.SWITCH_LEFT);
						} else {
							southB_Road.acquire();
							tsi.setSwitch(switch_Bottom_Left.x, switch_Bottom_Left.y, TSimInterface.SWITCH_RIGHT);
						}
					}
				}
				break;
				
			// The logic for this case is try to go around a corner
			// If the semaphore (critical section) is taken by the other train
			// it will stop for 3 seconds to let the train leave the corner
			// then acquire the semaphore to make sure the other train does not go around the same corner
			// and release the critical section it just traveled so the other train can take it.
			// It also changes the switch to make sure the train can pass through it.
			case 10:
				if(way == Way.North && currentSensor == SensorEvent.ACTIVE) {
					tsi.setSwitch(switch_Bottom_Left.x, switch_Bottom_Left.y, TSimInterface.SWITCH_LEFT);
					if(!west_Road.tryAcquire()){
						tsi.setSpeed(train_id, 0);
						TimeUnit.SECONDS.sleep(3);
						tsi.setSpeed(train_id, speed);
						southA_Road.release();
						west_Road.tryAcquire();
						tsi.setSwitch(switch_Bottom_Left.x, switch_Bottom_Left.y, TSimInterface.SWITCH_LEFT);
					}else {
						southA_Road.release();
					}
				}
				break;
				
			// The logic for this case is try to go around a corner
			// If the semaphore (critical section) is taken by the other train
			// it will stop for 3 seconds to let the train leave the corner
			// then acquire the semaphore to make sure the other train does not go around the same corner
			// and release the critical section it just traveled so the other train can take it.
			// It also changes the switch to make sure the train can pass through it.
			case 11:
				if(way == Way.North && currentSensor == SensorEvent.ACTIVE) {
					tsi.setSwitch(switch_Bottom_Left.x, switch_Bottom_Left.y, TSimInterface.SWITCH_RIGHT);
					if(!west_Road.tryAcquire()){
						tsi.setSpeed(train_id, 0);
						TimeUnit.SECONDS.sleep(3);
						tsi.setSpeed(train_id, speed);
						southB_Road.release();
						west_Road.tryAcquire();
						tsi.setSwitch(switch_Bottom_Left.x, switch_Bottom_Left.y, TSimInterface.SWITCH_RIGHT);
					}else {
						southB_Road.release();
					}
				}
				break;
				
			// A-Upper Train Station
			// When the train passes this sensor means it got to the station and it calls the method to stop it
			case 12:
				if(way == Way.North) {
					stopTrain();
				} 
				break;
				
			// B-Upper Train Station
			// When the train passes this sensor means it got to the station and it calls the method to stop it				
			case 13:
				if(way == Way.North) {
					stopTrain();
				}
				break;
				
			// B-Bottom Train Station
			// When the train passes this sensor means it got to the station and it calls the method to stop it
			case 14:
				if(way == Way.South) {
					stopTrain();
				}
				break;
				
			// A-Bottom Train Station
			// When the train passes this sensor means it got to the station and it calls the method to stop it
			case 15:
				if(way == Way.South) {
					stopTrain();
				}
				break;
			
			// Intersection Sensors
				
			// This case depending on the direction releases the critical section just finished
			// crossing, and it also makes sure it can cross in the intersection
			// if not, then waits until the train crossed the intersection. It acquires the semaphore
			// to protect the intersection
			case 16:
				if(way == Way.North) {
					north_Intersection.release();
				} else if(!north_Intersection.tryAcquire() && currentSensor == SensorEvent.ACTIVE){
					tsi.setSpeed(train_id, 0);
					TimeUnit.SECONDS.sleep(1);
					tsi.setSpeed(train_id, speed);
					north_Intersection.tryAcquire();
				}
				break;
				
			// This case depending on the direction releases the critical section just finished
			// crossing, and it also makes sure it can cross in the intersection
			// if not, then waits until the train crossed the intersection. It acquires the semaphore
			// to protect the intersection
			case 17: 
				if(way == Way.South) {
					north_Intersection.release();
				} else if(!north_Intersection.tryAcquire() && currentSensor == SensorEvent.ACTIVE){
					tsi.setSpeed(train_id, 0);
					TimeUnit.SECONDS.sleep(1);
					tsi.setSpeed(train_id, speed);
					north_Intersection.tryAcquire();
				}
				break;
				
			// This case depending on the direction releases the critical section just finished
			// crossing, and it also makes sure it can cross in the intersection
			// if not, then waits until the train crossed the intersection. It acquires the semaphore
			// to protect the intersection
			case 18:
				if(way == Way.North) {
					north_Intersection.release();
				} else if(!north_Intersection.tryAcquire() && currentSensor == SensorEvent.ACTIVE){
					tsi.setSpeed(train_id, 0);
					TimeUnit.SECONDS.sleep(1);
					tsi.setSpeed(train_id, speed);
					north_Intersection.tryAcquire();
				}
				break;
				
			// This case depending on the direction releases the critical section just finished
			// crossing, and it also makes sure it can cross in the intersection
			// if not, then waits until the train crossed the intersection. It acquires the semaphore
			// to protect the intersection
			case 19: 
				if(way == Way.South) {
					north_Intersection.release();
				} else if(!north_Intersection.tryAcquire() && currentSensor == SensorEvent.ACTIVE){
					tsi.setSpeed(train_id, 0);
					TimeUnit.SECONDS.sleep(1);
					tsi.setSpeed(train_id, speed);
					north_Intersection.tryAcquire();
				}
				break;
			}
			
		}
		
		// The method allows us to make the train full stop for two seconds
		// and also setting the speed on a negative value if the train needs to go on reverse
		// and viceversa when it comes from a negative speed it is turned to positive
		public void stopTrain() throws CommandException, InterruptedException {
			tsi.setSpeed(train_id, 0);
			TimeUnit.SECONDS.sleep(2);
			if(way == Way.North) {
				way = Way.South;
			}else {
				way = Way.North;
			}
			speed *= -1;
			tsi.setSpeed(train_id, speed);
		}
		
	}
}

public class java_time {
    protected static int NumThreads = 500;

    private static void usage() {
        System.out.println("proc_time takes at most one parameter.\n" +
"Usage is nprocs n, where n defines the number of processes to be created.\n" +
"If you don't specifiy n " + NumThreads + " processes will be created.\n" +
"Of course nprocs must be an integer\n");
        return;
        }
    
    public static void main(String[] args) {
        if (args.length > 1) {
            usage();
            System.exit(-1);
        } else if (args.length == 1) {
            NumThreads = Integer.valueOf(args[0]).intValue();            
        }

		long StartTime = System.nanoTime();
		for (int i=0;i<NumThreads;i++) {
			// spawn Thread. Thread exits immideately
			( new Thread() {
			public void run() {
			Thread.yield();
			}
			}
			).start();
		}
		long EndTime = System.nanoTime();
		long ThreadTime = EndTime - StartTime;
		
		//output
		double result = ((double)(ThreadTime)/(double)NumThreads) / 1000;
		System.out.println("Needed " + ThreadTime + " microseconds for spawning " + NumThreads + " threads. Mean time for thread creation: " + result + " microseconds");
	}

}

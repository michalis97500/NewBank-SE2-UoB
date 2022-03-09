package newbank.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class ExampleClient extends Thread{
	
	private Socket server;
	private PrintWriter bankServerOut;	
	private BufferedReader userInput;
	private Thread bankServerResponceThread;
	
	public ExampleClient(String ip, int port) throws IOException {
		server = new Socket(ip,port);
		userInput = new BufferedReader(new InputStreamReader(System.in)); 
		bankServerOut = new PrintWriter(server.getOutputStream(), true); 
		
		bankServerResponceThread = new Thread() {
			private BufferedReader bankServerIn = new BufferedReader(new InputStreamReader(server.getInputStream())); 
			@Override
			public void run() {
				try {
					while(true) {
						String responce = bankServerIn.readLine();
						System.out.println(responce);
					}
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		};
		bankServerResponceThread.start();
	}
	
	
	@Override
	public void run() {
		while(true) {
			try {
				while(true) {
					String command = userInput.readLine();
					bankServerOut.println(command);
				}				
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	public static void main(String[] args) throws IOException {
		new ExampleClient("localhost",14002).start();
	}
}

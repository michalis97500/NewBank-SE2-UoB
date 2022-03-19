package newbank.client;

import java.io.BufferedReader;
import java.io.Console;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class ExampleClient extends Thread{
	
	private Socket server;
	private PrintWriter bankServerOut;	
	private BufferedReader userInput;
	private Thread bankServerResponceThread;
	public Boolean passwordInput = false;


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
						switch(responce) {
							case "Enter Password":
								passwordInput = true;
								Console console = System.console();
								char[] passarray = console.readPassword();
								bankServerOut.println(new String(passarray));
								passwordInput = false;
								break;
							case "CLIENT_CLOSE_COMMAND":
								System.exit(0);
								break;
							default:
								break;
						}
					}
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		};
		bankServerResponceThread.start();
	}

	@Override
	public void run(){
			while(true) {
				try {
					if(Boolean.FALSE.equals(passwordInput)){
						String command = userInput.readLine();
						bankServerOut.println(command);
					}
					Thread.sleep(100);
				} catch (IOException | InterruptedException e) {
					e.printStackTrace();
				}
			}
	}

	
	public static void main(String[] args) throws IOException {
		new ExampleClient("localhost",14002).start();
	}
}

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
	private Boolean passwordInput = false;
	private String salt = "";


	public ExampleClient(String ip, int port) throws IOException {
		server = new Socket(ip,port);
		userInput = new BufferedReader(new InputStreamReader(System.in)); 
		bankServerOut = new PrintWriter(server.getOutputStream(), true); 
		bankServerResponceThread = new Thread() {
			private BufferedReader bankServerIn = new BufferedReader(new InputStreamReader(server.getInputStream())); 
			@Override
			public void run() {
				try {
					Console console = System.console();
					char[] passarray = null;
					while(true) {
						String responce = bankServerIn.readLine();
						System.out.println(responce);
						switch(responce) {
							case "Please enter your current password :":
							case "Enter Password":
								salt = bankServerIn.readLine();
								if(!salt.equals("FAIL") && salt.length() > 5){
								passwordInput = true;
								passarray = console.readPassword();
								bankServerOut.println(BCrypt.hashpw(new String(passarray), salt));
								passarray = null;
								salt = null;
								passwordInput = false;
								}
								break;
							case "Please enter a new password :":
								passwordInput = true;
								salt = BCrypt.gensalt();
								Boolean goodPass = false;
								String passHashConfirm = null;
								String passHash= null;
								while(!goodPass){
									passarray = console.readPassword();
									if(checkPass(new String(passarray))){
										passHash = BCrypt.hashpw(new String(passarray), salt);
										goodPass = true;
									}
									passarray = null;
									if(!goodPass){
										System.out.println("Please re-enter a new password :");
									}
								}
								System.out.println("\nPlease confirm the password :");
								passarray = console.readPassword();
								passHashConfirm = BCrypt.hashpw(new String(passarray), salt);
								passarray = null;
								if(passHash != null && passHashConfirm != null && passHash.equals(passHashConfirm) ){
									bankServerOut.println(passHash + " __SALT__ " + salt);
									passwordInput = false;
									break;
								}
								System.out.println("Passwords do not match.");
								bankServerOut.println(" ");
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

	private boolean checkPass(String password){
			try {
				if(password.length() < 8){
					System.out.println("Error : Password minimum length is 8.");
					return false;
				}
				if(!password.matches("(?=.*[0-9]).*") ){
					System.out.println("Error : Password must contain at least 1 digit");
					return false;
				}
				if(!password.matches("(?=.*[A-Z]).*") ){
					System.out.println("Error : Password must contain at least 1 uppercase letter");
					return false;
				}
				if(!password.matches("(?=.*[a-z]).*") ){
					System.out.println("Error : Password must contain at least 1 lowercase letter");
					return false;
				}
				if(!password.matches("(?=.*[~!@#$%^&*()_-]).*") ){
					System.out.println("Error : Password must contain at least 1 special character");
					return false;
				}
			return true;
			} catch (Exception e) {
				e.printStackTrace();
				return false;
		}
	
	}
	
	public static void main(String[] args) throws IOException {
		new ExampleClient("localhost",14002).start();
	}
}

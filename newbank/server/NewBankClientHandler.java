package newbank.server;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.Map;

public class NewBankClientHandler extends Thread{
	
	private NewBank bank;
	private BufferedReader in;
	private PrintWriter out;
	private String checkings = "Cheking";
	private String savings = "Savings";
	
	
	public NewBankClientHandler(Socket s) throws IOException {
		bank = NewBank.getBank();
		in = new BufferedReader(new InputStreamReader(s.getInputStream()));
		out = new PrintWriter(s.getOutputStream(), true);
	}

	private void printInterfaceOption(){
		//Added by M. Christou
		out.println("1. Show all accounts inforamtion - SHOWMYACCOUNTS");
		out.println("2. Create account - NEWACCOUNT <Name>");
		out.println("3. Pay person/entity - PAY <Entity> <Ammount>");
		out.println("4. Transfer funds between accounts - MOVE <Amount> <From> <To>");
		out.println("You may navigate the menu by entering the number or using the commands.");
	}

	private String accountCreation(){ //Method by M.Christou
		String error = "WRONGACCOUNTTYPE";
		try{
			//Add a new account.
			out.println("Please enter account type (number)");
			out.println("1. Savings account");
			out.println("2. Checking account");
			String accountType = in.readLine();
			switch(accountType){
				case "1" :
					accountType = savings;
					break;
				case "2" :
					accountType = checkings;
					break;
				case "Savings" :
					accountType = savings;
					break;
				case "Checking" :
					accountType = checkings;
					break;
				default:
					accountType = error;
					break;
			}
			return accountType;
		} catch (Exception e){
			out.println("Error in account creation");
			e.printStackTrace();
			return error;
		}
	}
	
	private String paymentBuilder(CustomerID myCustomerID){ //Method by M.Christou
		try{
			String error = "INVALID_INPUT";
			//Add a new account.
			out.println("Please enter the entity to pay. This can be a person or a buisiness");
			String beneficiary = in.readLine();
			if (beneficiary == null || beneficiary.isEmpty() || beneficiary.trim().isEmpty()){
				return error;
			}
			//check if beneficiary is member of the bank
			if(beneficiaryExists(beneficiary)){
				Double myBalance = bank.getAccountBalance(myCustomerID, "Main");
				out.println("Please enter the amount to pay. The available balance is : " + myBalance);
				String amount = in.readLine();
				if(amount == null || amount.isEmpty() || amount.trim().isEmpty()){
					return error;
				}
				if (Double.parseDouble(amount) > 0){
					return "PAY " + beneficiary + amount;
				}
				return error;
			}
			return error;
		} catch (Exception e){
			out.println("Error in payment proccess");
			e.printStackTrace();
			return "INVALID_INPUT";
		}
	}
	
	private boolean beneficiaryExists(String customer){
		Map<String,Customer> customerList = bank.getCustomerList();
		Customer beneficiaryCustomer;
		try{
			beneficiaryCustomer = customerList.get(customer);
			if (beneficiaryCustomer == null) {
				out.println("No such entity has been found in bank.");
				return false;
			}
			return true;
		} catch (Exception e) {
			out.println("No such entity has been found in bank.");
				return false;
		}

	}

	@Override
	public void run() { //Method modified by M.Christou for better UX
		// keep getting requests from the client and processing them
		try {
			// ask for user name
			out.println("Enter Username");
			String userName = in.readLine();
			// ask for password
			out.println("Enter Password");
			String password = in.readLine();
			out.println("Checking Details...");
			// authenticate user and get customer ID token from bank for use in subsequent requests
			CustomerID customer = bank.checkLogInDetails(userName, password);
			// if the user is authenticated then get requests from the user and process them 
			if(customer != null) {
				out.println("Log In Successful. What do you want to do?");
				printInterfaceOption(); //Added by M. Christou
				while(true) {
					String request = in.readLine();
					System.out.println("Request from " + customer.getKey());
					//boolean to check if command is valid
					Boolean validCommand = true;
					//break down customer requests
					String[] mainCommand = request.split(" ");
					switch(mainCommand[0]) {
						case "1" : 
							//Show all balances. Change the request to SHOWMYACCOUNTS
							request = "SHOWMYACCOUNTS";
							break;
						case "2" : 
							String accountType = accountCreation();
							request = "NEWACCOUNT " + accountType;
							if(accountType.equals("WRONGACCOUNTTYPE")){
								out.println("Wrong account type selection!");
								validCommand = false;
							}
							break;
						case "3" : 
							break;
						case "4" : 
							break;
						default:
					}
					if(Boolean.TRUE.equals(validCommand)){
						String responce = bank.processRequest(customer, request);
						out.println(responce);
					}
				}
			}
			else {
				//Added by M. Christou
				out.println("Username or password is incorrect. Please try again \n");
				Thread.currentThread().interrupt();
				run();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		finally {
			try {
				in.close();
				out.close();
			} catch (IOException e) {
				e.printStackTrace();
				Thread.currentThread().interrupt();
			}
		}
	}

}

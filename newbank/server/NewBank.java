package newbank.server;

import java.sql.*;
import java.util.HashMap;
import java.util.Map;

public class NewBank {
	
	private static final NewBank bank = new NewBank();
	private HashMap<String,Customer> customers;
	private Connection connection;
	
	private NewBank() {
		customers = new HashMap<>();
		addTestData();
	}

	public Map<String,Customer> getCustomerList(){
		return this.customers;
	}
	
	private void addTestData() {
		Customer bhagy = new Customer();
		bhagy.addAccount(new Account("Main", 1000.0));
		customers.put("Bhagy", bhagy);
		
		Customer christina = new Customer();
		christina.addAccount(new Account("Savings", 1500.0));
		customers.put("Christina", christina);
		
		Customer john = new Customer();
		john.addAccount(new Account("Checking", 250.0));
		customers.put("John", john);
	}
	
	public static NewBank getBank() {
		return bank;
	}
	
	public synchronized CustomerID checkLogInDetails(String userName, String password) {
		if(customers.containsKey(userName)) {
			return new CustomerID(userName);
		}
		return null;
	}

	// commands from the NewBank customer are processed in this method
	public synchronized String processRequest(CustomerID customer, String request) {
		String[] command = request.split(" ");
		if(customers.containsKey(customer.getKey())) {
			switch(command[0]) {
			case "SHOWMYACCOUNTS" : return showMyAccounts(customer);
			//Added by M. Christou
			case "NEWACCOUNT" :
				return newAccount(customer, command[1]);
			case "MOVE" :
				break;
			case "PAY" : 
				break;
			
			default : return "FAIL";
			}
		}
		return "FAIL";
	}
	
	private String showMyAccounts(CustomerID customer) {
		return (customers.get(customer.getKey())).accountsToString();
	}

	private String newAccount (CustomerID customer, String accountType){ //Method implemented by M. Christou
		customers.get(customer.getKey()).addAccount(new Account (accountType, 0.00));
		addNewAccount(customers.get(customer.getKey()), accountType);
		return "New account created successfully.\n";
	}

	public void addNewAccount (Customer customer, String accountType){ //Method implemented by M. Christou
		try{
			PreparedStatement sqlStatement;
			String statement;
			String balance = "0.0";
			switch(accountType)
			{
				case "Savings":
					statement = "UPDATE account_info SET savings=? WHERE username = ?";
					break;
				case "Checking":
					statement = "UPDATE account_info SET checking=? WHERE username = ?";
					break;
				default:
					statement = null;
					break;
			}
			if (statement != null){
				sqlStatement = connection.prepareStatement(statement);
				sqlStatement.setString(1, balance);
				sqlStatement.setString(2, getKeyFromValue(customers, customer));
				sqlStatement.executeUpdate();
			}
		} catch (Exception e){
			System.out.println("Error in new account addition : ");
			e.printStackTrace();
		}
	}

	public static <K, V> K getKeyFromValue(Map<K, Customer> map, V value){ //Method implemented by M.Christou
		for (Map.Entry<K, Customer> entry : map.entrySet()) {
			if (value.equals(entry.getValue())) {
				return entry.getKey();
			}
	  }
	  return null;
	}

	public double getAccountBalance(CustomerID customer, String accountType){//Method implemented by M.Christou
		try{
			String allBalances = customers.get(customer.getKey()).accountsToString();
			//allBalances should be of something like  "Main: 1000.0" 
			accountType = accountType.trim() + ": ";
			String[] balance = allBalances.split(accountType);
			//if accountType is of format "Main: " then split produces balance[1] = "1000.0 "
			String[] currentBalance = new String[10];
			if(balance.length > 0){
				currentBalance = balance[1].split(" ");
			}
			double balanceToReturn = 0;
			balanceToReturn = Double.parseDouble(currentBalance[0]);
			return balanceToReturn;
		} catch (Exception e){
			System.out.println("Error in getting account balance");
			e.printStackTrace();
			return 0;
		}
	}

}

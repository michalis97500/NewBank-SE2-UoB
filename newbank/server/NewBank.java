package newbank.server;

import java.util.HashMap;
import java.util.Map;

public class NewBank {

	private static final NewBank bank = new NewBank();
	private HashMap<String, Customer> customers;

	private NewBank() {
		customers = new HashMap<>();
		addTestData();
	}

	public Map<String, Customer> getCustomerList() {
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
		if (customers.containsKey(userName)) {
			return new CustomerID(userName);
		}
		return null;
	}

	public synchronized CustomerID getCustomerID(String userName) {
		if (customers.containsKey(userName)) {
			return new CustomerID(userName);
		}
		return null;
	}

	public synchronized boolean customerExists(String customer) {
		return customers.containsKey(customer);
	}

	// commands from the NewBank customer are processed in this method
	public synchronized String processRequest(CustomerID customerID, String request) {
		String[] command = request.split(" ");
		if (customers.containsKey(customerID.getKey())) {
			switch (command[0]) {
				case "SHOWMYACCOUNTS":
					return showMyAccounts(customerID);
				// Added by M. Christou
				case "NEWACCOUNT":
					return newAccount(customerID, command[1]);
				// Added by M. Christou
				case "MOVE":
					return transferMoney(customerID, command[2], command[3], command[1]);
				// Added by M. Christou
				case "PAY":
					return pay(command[1], customerID, command[2]);
				default:
					return "FAIL";
			}
		}
		return "FAIL";
	}

	private String showMyAccounts(CustomerID customerID) {
		Map<String, Double> accountsMap = customers.get(customerID.getKey()).getAccounts();
		StringBuilder accountList = new StringBuilder();
		accountList.append("\n");
		for (Map.Entry<String, Double> entry : accountsMap.entrySet()) {
			String accountName = entry.getKey();
			Double accountValue = entry.getValue();
			accountList.append(accountName + " : $" + accountValue.toString() + "\n");
		}
		return accountList.toString();
	}

	private String newAccount(CustomerID customerID, String accountType) { // Method implemented by M. Christou
		if (Boolean.FALSE.equals(accountExists(customerID, accountType))) {
			customers.get(customerID.getKey()).addAccount(new Account(accountType, 0.00));
			return "New account created successfully.\n";
		}
		return "Account already exists";
	}

	public static <K, V> K getKeyFromValue(Map<K, Customer> map, V value) { // Method implemented by M.Christou
		for (Map.Entry<K, Customer> entry : map.entrySet()) {
			if (value.equals(entry.getValue())) {
				return entry.getKey();
			}
		}
		return null;
	}

	public double getAccountBalance(CustomerID customerID, String accountType) {// Method implemented by M.Christou
		try {
			double balanceToReturn = 0;
			// Check account exists
			if (Boolean.TRUE.equals(accountExists(customerID, accountType))) {
				Map<String, Double> accountsMap = customers.get(customerID.getKey()).getAccounts();
				return accountsMap.get(accountType);
			}
			return balanceToReturn;
		} catch (Exception e) {
			System.out.println("Error in getting account balance");
			e.printStackTrace();
			return 0;
		}
	}

	private String pay(String beneficiary, CustomerID customerID, String amount) { // Method by M.Christou
		// We should have 3 seperate strings here.
		double payment;
		CustomerID beneficiaryID = null;
		// Check amount is correct
		try {
			payment = Double.parseDouble(amount);
			// check for negative numbers
			if (payment <= 0) {
				return "Error : Amount entered is negative.";
			}
		} catch (NumberFormatException e) {
			return "Error : Amount entered must be numbers only.";
		}

		// Check beneficiary exists
		try {
			beneficiaryID = getCustomerID(beneficiary);
			if (beneficiaryID == null) {
				return "Beneficiary does not exist in bank.";
			}
		} catch (Exception e) {
			e.printStackTrace();
			return "Beneficiary does not exist in bank.";
		}

		// Check beneficiary has Main account
		try {
			if (Boolean.FALSE.equals(accountExists(beneficiaryID, "Main"))) {
				return "Beneficiary cannot accept payments";
			}
		} catch (Exception e) {
			e.printStackTrace();
			return "Beneficiary cannot accept payments.";
		}

		// Check balance is sufficient
		double balance = getAccountBalance(customerID, "Main");
		if (balance >= payment) {
			// Deduct from balance
			transaction(customerID, "Main", -payment);
			// check deduction is correct
			double balanceAfter = getAccountBalance(customerID, "Main");
			if (balance - balanceAfter == payment) {
				double beneficiarybalance = getAccountBalance(beneficiaryID, "Main");
				// Add to beneficiary
				transaction(beneficiaryID, "Main", payment);
				double beneficiarybalanceAfter = getAccountBalance(beneficiaryID, "Main");
				if (beneficiarybalanceAfter - beneficiarybalance == payment) {
					return "SUCCESS";
				}
				return "Error in sending money. Please contact your banker.";
			}
			return "Error in balance deduction check. Please contact your banker.";
		}
		return "Balance in Main account insufficient.";
	}

	private String transaction(CustomerID customerID, String accountType, Double amount) { // Method implemented by
																																													// M.Christou
		return customers.get(customerID.getKey()).modifyBalance(accountType, amount);
	}

	public Boolean accountExists(CustomerID customerID, String accountType) {// Method implemented by M.Christou
		try {
			Map<String, Double> accountsMap = customers.get(customerID.getKey()).getAccounts();
			boolean doesExist = false;
			for (String accountName : accountsMap.keySet()) {
				if (accountName.equals(accountType)) {
					doesExist = true;
				}
			}
			return doesExist;

		} catch (Exception e) {
			return false;
		}
	}

	private String transferMoney(CustomerID customerID, String fromaccountType, String toaccountType, String amount) { // Method
																																																											// implemented
																																																											// by
																																																											// M.Christou
		double payment;
		// Check amount is correct
		try {
			payment = Double.parseDouble(amount);
			// check for negative numbers
			if (payment <= 0) {
				return "Error : Amount entered is negative.";
			}
		} catch (NumberFormatException e) {
			return "Error : Amount entered must be numbers only.";
		}

		// Check if accounts exist
		try {
			if (Boolean.FALSE.equals(accountExists(customerID, fromaccountType))) {
				return "Account \"" + fromaccountType + "\" does not exist";
			}
			if (Boolean.FALSE.equals(accountExists(customerID, toaccountType))) {
				return "Account \"" + toaccountType + "\" does not exist";
			}

			if (payment <= 0) {
				return "Account does not exist";
			}
		} catch (Exception e) {
			return "Account does not exist";
		}

		// Check if account has enough balance
		Double currentBalance = getAccountBalance(customerID, fromaccountType);
		if (currentBalance >= payment) {
			// Remove the amount from first account
			if (transaction(customerID, fromaccountType, -payment).equals("SUCCESS")) {
				return transaction(customerID, toaccountType, payment);
			}
			return "FAIL";
		} else {
			return "Balance in \"" + fromaccountType + "\" account insufficient.";
		}

	}
}

package newbank.server;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class NewBankClientHandler extends Thread {

	private NewBank bank;
	private BufferedReader in;
	private PrintWriter out;
	private String checkings = "Checking";
	private String savings = "Savings";
	private String error = "INVALID_INPUT";
	private String cancel = "CANCEL";

	public final void clearScreen(String prompt) { // Method implemented by M. Christou
		try {
			out.println("\033[H\033[2J");
			out.flush();
			if (prompt != null) {
				out.println(prompt);
			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void mainMenu() { // Method implemented by M. Christou
		try {
			out.println("Press any key to return to main menu...");
			in.read();
			clearScreen(null);
			printInterfaceOption();
		} catch (Exception e) {
			e.printStackTrace();
			out.println("Main menu software error");
		}
	}

	public String changePassword(String customerID) {// Method implemented by M. Christou
		try {
			clearScreen("Please enter your current password :");
			out.println(getCurrentSalt(customerID));
			String oldPassHash = in.readLine();
			clearScreen("Please enter a new password :");
			String newPassHash = in.readLine();
			if (!newPassHash.contains("__SALT__")) {
				return error;
			}
			String[] split = newPassHash.split(" __SALT__ ");
			return "CHANGEPASS " + customerID + " " + oldPassHash + " " + split[0] + " " + split[1];
		} catch (Exception e) {
			e.printStackTrace();
			return error;
		}
	}

	private String getCurrentSalt(String customerID) {
		return bank.processRequest(customerID, "GETCURRENTSALT");
	}

	public NewBankClientHandler(Socket s) throws IOException {
		bank = NewBank.getBank();
		in = new BufferedReader(new InputStreamReader(s.getInputStream()));
		out = new PrintWriter(s.getOutputStream(), true);
	}

	private void printInterfaceOption() {
		// Added by M. Christou
		out.println("1. Show all accounts inforamtion - SHOWMYACCOUNTS");
		out.println("2. Create account - NEWACCOUNT <Name>");
		out.println("3. Pay person/entity - PAY <Entity> <Ammount>");
		out.println("4. Transfer funds between accounts - MOVE <Amount> <From> <To>");
		out.println("5. Loan request"); // added by ycanli
		out.println("6. Show my loan account - SHOWMYLOANACCOUNT"); // added by H. Chan
		out.println("7. Logout");
		out.println("8. Exit");
		out.println("9. Change password");
		out.println("You may navigate the menu by entering the number or using the commands.");
	}

	private String accountCreation() { // Method by M.Christou
		try {
			// Add a new account.
			out.println("Please enter account type (number)");
			out.println("1. Savings account");
			out.println("2. Checking account");
			out.println("3. Main account");
			out.println("4. Cancel and return to main menu");
			String accountType = in.readLine();
			switch (accountType) {
				case "1":
				case "Savings":
					accountType = savings;
					break;
				case "2":
				case "Checking":
					accountType = checkings;
					break;
				case "3":
				case "Main":
					accountType = "Main";
					break;
				case "4":
				case "Cancel":
				case "cancel":
				case "CANCEL":
					return "SYSTEM_CANCEL";
				default:
					accountType = error;
					break;
			}
			return accountType;
		} catch (Exception e) {
			out.println("Error in account creation");
			e.printStackTrace();
			return error;
		}
	}

	private String moveBuilder(String customerID) { // Method by M.Christou
		String fromaccountType = null;
		String toaccountType = null;
		String amount = "";
		String responce;
		String[] accountsFrom;
		String[] accountsTo;
		StringBuilder accountDisplay;
		Boolean accountFromBool = false;
		Boolean accountToBool = false;
		Boolean amountBool = false;
		Double myBalance = 0.00;
		String errorString = "";
		int i = 1;

		// Select account to pay from
		while (Boolean.FALSE.equals(accountFromBool)) {
			try {
				out.println("To cancel at any time, please write \"Cancel\"");
				responce = bank.processRequest(customerID, "SHOWMYACCOUNTS");
				accountsFrom = responce.split("\n");
				accountDisplay = new StringBuilder();
				for (String account : accountsFrom) {
					accountDisplay.append(Integer.toString(i) + ". " + account + "\n");
					i++;
				}
				out.println(accountDisplay.toString());
				i = 1;
			} catch (Exception e) {
				out.println("Error in account display, aborting");
				e.printStackTrace();
				return error;
			}
			try {
				out.println("Please select the account to pay from:");
				String accountSelection = in.readLine();
				errorString = accountSelection;
				if (accountSelection.equalsIgnoreCase(cancel)) {
					return error;
				}
				if (accountSelection.length() == 1 && accountsFrom != null && accountsFrom.length > 0) {
					String accountByNumber = accountsFrom[Integer.parseInt(accountSelection) - 1];
					String[] accountSplit = accountByNumber.split(" ");
					accountSelection = accountSplit[0];
				}
				if (Boolean.TRUE.equals(bank.accountExists(customerID, accountSelection.trim()))) {
					fromaccountType = accountSelection.trim();
					myBalance = bank.getAccountBalance(customerID, fromaccountType);
					if (myBalance > 0) {
						out.println("Account " + fromaccountType + " selected. \n");
						accountFromBool = true;
					} else {
						clearScreen("Account " + fromaccountType + " has no balance. Please choose another account.\n");
					}

				} else {
					clearScreen("Error, \"" + errorString + "\" is not a valid account, please try again: \n");
				}
			} catch (Exception e) {
				clearScreen("Error, \"" + errorString + "\" is not a valid account, please try again: \n");
				e.printStackTrace();
			}
		}
		clearScreen("Account " + fromaccountType + " selected as source of transfer funds. \n");
		// Select account to pay to
		while (Boolean.FALSE.equals(accountToBool)) {
			try {

				out.println("To cancel at any time, please input \"Cancel\"");
				responce = bank.processRequest(customerID, "SHOWMYACCOUNTS");
				StringBuilder accountDisplay2 = new StringBuilder();
				String[] accountsToAll = responce.split("\n");
				accountsTo = new String[accountsToAll.length - 1];
				for (String account : accountsToAll) {
					if (!account.contains(fromaccountType)) {
						accountDisplay2.append(Integer.toString(i) + ". " + account + "\n");
						accountsTo[i - 1] = account;
						i++;

					}
				}
				out.println(accountDisplay2.toString());
				i = 1;
			} catch (Exception e) {
				out.println("Error in account display, aborting");
				e.printStackTrace();
				return error;
			}
			try {
				out.println("Please select the account to pay to:");
				String accountSelection = in.readLine();
				errorString = accountSelection;
				if (accountSelection.equalsIgnoreCase(cancel)) {
					return error;
				}
				if (accountSelection.length() == 1 && accountsTo != null && accountsTo.length > 0) {
					String accountByNumber = accountsTo[Integer.parseInt(accountSelection) - 1];
					String[] accountSplit = accountByNumber.split(" ");
					accountSelection = accountSplit[0];
				}
				if (Boolean.TRUE.equals(bank.accountExists(customerID, accountSelection.trim()))) {
					toaccountType = accountSelection.trim();
					out.println("Account " + toaccountType + " selected. \n");
					accountToBool = true;
				} else {
					clearScreen("Error, \"" + errorString + "\" is not a valid account, please try again: \nAccount "
							+ fromaccountType + " selected as source of transfer funds. \n");
				}
			} catch (Exception e) {
				clearScreen("Error, \"" + errorString + "\" is not a valid account, please try again: \nAccount "
						+ fromaccountType + " selected as source of transfer funds. \n");
				e.printStackTrace();
			}
		}

		clearScreen("Please enter the amount to move. The available balance is : " + myBalance);
		while (Boolean.FALSE.equals(amountBool)) {
			// Get account balance
			try {

				out.println("To cancel at any time, please input \"Cancel\"");
				amount = in.readLine();
				if (amount == null || amount.isEmpty() || amount.trim().isEmpty()) {
					clearScreen("No amount entered.\nPlease enter the amount to move. The available balance is : " + myBalance);
				}
				if (amount != null && amount.equalsIgnoreCase(cancel)) {
					return error;
				}
			} catch (Exception e) {
				clearScreen("No amount entered.\nPlease enter the amount to move. The available balance is : " + myBalance);
			}

			// Make sure amount is parsable
			try {
				Double payment = Double.parseDouble(amount);
				// check for negative numbers
				if (payment < 0) {
					clearScreen("Amount must be greater than 0.00.\nPlease enter the amount to move. The available balance is : "
							+ myBalance);
				}
				if (payment > 0) {
					amountBool = true;
				}

			} catch (NumberFormatException e) {
				clearScreen(
						"Invalid format, please use numbers only.\nPlease enter the amount to move. The available balance is : "
								+ myBalance);
			}
		}

		if (fromaccountType != null && toaccountType != null) {
			clearScreen("Confirm transfer of $" + amount + " from account \"" + fromaccountType + "\" to account \""
					+ toaccountType + "\"");
			out.println("1. Confirm");
			out.println("2. Reject");
			String confirmation;
			try {
				confirmation = in.readLine();
				switch (confirmation) {
					case "1":
					case "Confirm":
					case "Yes":
						return "MOVE " + amount + " " + fromaccountType + " " + toaccountType;
					default:
						out.println("Action has been cancelled");
						return error;
				}
			} catch (IOException e) {
				e.printStackTrace();
			}

		}
		return error;
	}

	private String paymentBuilder(String customerID) { // Method by M.Christou
		String accountType = "Main";
		String amount;
		String beneficiary;
		// Get beneficiary
		try {
			out.println("To cancel at any time, please input \"CANCEL\"");
			out.println("Please enter the beneficiary to pay:");
			beneficiary = in.readLine();
			if (beneficiary == null || beneficiary.isEmpty() || beneficiary.trim().isEmpty()) {
				out.println("Beneficiary is empty, aborting.");
				return error;
			}
			if (beneficiary.equalsIgnoreCase(cancel)) {
				out.println("Cancelling...");
				return error;
			}
			if (!bank.userNameExists(beneficiary)) {
				out.println("Beneficiary does not exist in bank, aborting.");
				return error;
			}
		} catch (Exception e) {
			out.println("Error in beneficiary selection, aborting.");
			e.printStackTrace();
			return error;
		}

		// Get account balance
		try {
			Double myBalance = bank.getAccountBalance(customerID, accountType);
			out.println("Please enter the amount to pay. The available balance is : " + myBalance);
			amount = in.readLine();
			if (amount == null || amount.isEmpty() || amount.trim().isEmpty()) {
				out.println("No amount entered, aborting.");
				return error;
			}
			if (amount.equalsIgnoreCase(cancel)) {
				out.println("Cancelling...");
				return error;
			}
		} catch (Exception e) {
			out.println("Error in amount");
			e.printStackTrace();
			return error;
		}

		// Make sure amount is parsable
		try {
			Double payment = Double.parseDouble(amount);
			// check for negative numbers
			if (payment < 0) {
				out.println("Amount entered is negative, aborting.");
				return error;
			}
		} catch (NumberFormatException e) {
			out.println("Amount entered must be numbers only, aborting.");
			return error;
		}

		clearScreen("Confirm transfer of $" + amount + " from Main account to beneficiary \"" + beneficiary + "\"");
		out.println("1. Confirm");
		out.println("2. Reject");
		String confirmation;
		try {
			confirmation = in.readLine();
			switch (confirmation) {
				case "1":
				case "Confirm":
				case "Yes":
					return "PAY " + beneficiary + " " + amount;
				default:
					out.println("Action has been cancelled");
					return error;
			}
		} catch (IOException e) {
			e.printStackTrace();
			return error;
		}

	}

	public String loanBuilder(String customerID) // Cathy + Agnes
	{
		String loanAmount;
		String loanPeriodDays;
		
		// Request loan amount from the user 
		try {
			out.println("To cancel at any time, please input \"CANCEL\"");
			
			out.println("Please enter the loan amount you would like to borrow:");
			loanAmount = in.readLine();
			if (loanAmount == null || loanAmount.isEmpty() || loanAmount.trim().isEmpty()) {
				out.println("Loan amount is empty, aborting.");
				return error;
			}
			if (loanAmount.equalsIgnoreCase(cancel)) {
				out.println("Cancelling...");
				return error;
			}
		} catch (Exception e) {
			out.println("Error in loan amount requested, aborting.");
			e.printStackTrace();
			return error;
			}
		// Request number of days for the loan from the user
		try {
			clearScreen("To cancel at any time, please input \"CANCEL\"");
			out.println("Please enter your preferred loan repayment time period in days:");
			loanPeriodDays = in.readLine();
			if (loanPeriodDays == null || loanPeriodDays.isEmpty() || loanPeriodDays.trim().isEmpty()) {
				out.println("No loan repayment time period specified, aborting.");
				return error;
			}
			if (loanPeriodDays.equalsIgnoreCase(cancel)) {
				out.println("Cancelling...");
				return error;
			}	
		} catch (Exception e) {
			out.println("Error in loan repayment time request, aborting.");
			e.printStackTrace();
			return error;
			}
		
		//Offer the interest rates to the user		

			if(Integer.parseInt(loanPeriodDays) <= 30){	
				Double interestRate = 15.0;
				Double repaymentAmount = Double.parseDouble(loanAmount) * (1 + interestRate / 100);
				clearScreen("Loan requested is for $" + loanAmount + " for " + loanPeriodDays + " days. The interest rate is 15% making the total repayment amount to New Bank $" + repaymentAmount + ". Would you like to confirm?\n");
			}else{
				Double interestRate = 20.0;
				Double repaymentAmount = Double.parseDouble(loanAmount) * (1 + interestRate / 100);
				clearScreen("Loan requested is for $" + loanAmount + " for " + loanPeriodDays + " days. The interest rate is 20% making the total repayment amount to New Bank $" + repaymentAmount + ". Would you like to confirm? \n");
			}

		out.println("1. Confirm");
		out.println("2. Reject");
		String confirmation;
		try {
			confirmation = in.readLine();
			switch (confirmation) {
				case "1":
				case "Confirm":
				case "Yes":
					return "LOAN " + customerID  + " " + loanAmount  + " " + loanPeriodDays;
				default:
					out.println("Action has been cancelled");
					return error;
			}
		} catch (IOException e) {
			e.printStackTrace();
			return error;
		}

	}
	
	@Override
	public void run() { // Method modified by M.Christou for better UX
		// keep getting requests from the client and processing them
		try {
			// ask for user name
			out.println("Enter Username");
			String userName = in.readLine();
			// ask for password
			out.println("Enter Password");
			out.println(getCurrentSalt(bank.getCustomerID(userName)));
			String password = in.readLine();
			out.println("Checking Details...");
			// authenticate user and get customer ID token from bank for use in subsequent
			// requests
			String customerID = bank.checkLogInDetails(userName, password);
			// if the user is authenticated then get requests from the user and process them
			if (customerID != null) {
				clearScreen(null);
				out.println("Log In Successful. What do you want to do?");
				out.print("\n");
				printInterfaceOption(); // Added by M. Christou
				while (true) {
					String request = in.readLine();
					System.out.println("Request from ID: " + customerID);
					// boolean to check if command is valid
					Boolean validCommand = true;
					// break down customer requests
					String[] mainCommand = request.split(" ");
					switch (mainCommand[0]) {
						case "1":
							clearScreen(null);
							request = "SHOWMYACCOUNTS";
							break;
						case "2":
							clearScreen(null);
							String accountType = accountCreation();
							request = "NEWACCOUNT " + accountType;
							if (accountType.equals(error)) {
								out.println("Wrong account type selection!");
								validCommand = false;
								mainMenu();
							}
							if (accountType.equals("SYSTEM_CANCEL")) {
								validCommand = false;
								break;
							}
							break;
						case "3":
							clearScreen(null);
							request = paymentBuilder(customerID);
							if (request.equals(error)) {
								validCommand = false;
								mainMenu();
							}
							break;
						case "4":
							clearScreen(null);
							request = moveBuilder(customerID);
							if (request.equals(error)) {
								validCommand = false;
								mainMenu();
							}
							break;
						case "5":
						case "Loan":
							clearScreen(null);
							request = loanBuilder(customerID);
							if (request.equals(error)) {
								validCommand = false;
								mainMenu();
							}
							break;
						case "6":
							clearScreen(null);
							request = "SHOWACTIVELOANINFO";
							break;	
						case "7":
						case "Logout":
							clearScreen(null);
							Thread.currentThread().interrupt();
							run();
							break;
						case "8":
						case "Exit":
							out.println("CLIENT_CLOSE_COMMAND");
							break;
						case "9":
							request = changePassword(customerID);
							if (request.equals(error)) {
								validCommand = false;
								out.println("Password has not been changed.");
								mainMenu();
							}
							break;
						case "SHOWMYACCOUNTS":
						case "SHOWMYLOANACCOUNT":
						case "NEWACCOUNT":
						case "MOVE":
						case "PAY":
							validCommand = true;
							break;
						default:
							validCommand = false;
							break;
					}
					if (Boolean.TRUE.equals(validCommand)) {
						String responce = bank.processRequest(customerID, request);
						out.println(responce);
						mainMenu();
					}
					clearScreen(null);
					printInterfaceOption();
				}
			} else {
				// Added by M. Christou
				out.println("Username or password is incorrect. Please try again \n");
				Thread.currentThread().interrupt();
				run();
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
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

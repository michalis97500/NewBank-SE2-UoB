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

	public final void clearScreen() {
		try {
			out.print("\033[H\033[2J");
			out.flush();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void mainMenu() {
		try {
			out.println("Press any key to return to main menu...");
			in.read();
			clearScreen();
			printInterfaceOption();
		} catch (Exception e) {
			e.printStackTrace();
			out.println("Main menu software error");
		}
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
		out.println("5. Logout");
		out.println("You may navigate the menu by entering the number or using the commands.");
	}

	private String accountCreation() { // Method by M.Christou
		try {
			// Add a new account.
			out.println("Please enter account type (number)");
			out.println("1. Savings account");
			out.println("2. Checking account");
			out.println("3. Main account");
			String accountType = in.readLine();
			switch (accountType) {
				case "1":
					accountType = savings;
					break;
				case "2":
					accountType = checkings;
					break;
				case "3":
					accountType = "Main";
					break;
				case "Main":
					accountType = "Main";
					break;
				case "Savings":
					accountType = savings;
					break;
				case "Checking":
					accountType = checkings;
					break;
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
		String amount;
		// Load user accounts
		try {
			String responce = bank.processRequest(customerID, "SHOWMYACCOUNTS");
			out.println(responce);
		} catch (Exception e) {
			out.println("Error in account display, aborting");
			e.printStackTrace();
			return error;
		}

		// Select account to pay from
		try {
			out.println("Please select the account to pay from:");
			String accountSelection = in.readLine();
			if (Boolean.TRUE.equals(bank.accountExists(customerID, accountSelection.trim()))) {
				fromaccountType = accountSelection.trim();
			}
		} catch (Exception e) {
			out.println("Error in account selection, aborting");
			e.printStackTrace();
			return error;
		}

		// Select account to pay to
		try {
			out.println("Please select the account to pay to:");
			String accountSelection = in.readLine();
			if (Boolean.TRUE.equals(bank.accountExists(customerID, accountSelection.trim()))) {
				toaccountType = accountSelection.trim();
			}
		} catch (Exception e) {
			out.println("Error in account selection, aborting");
			e.printStackTrace();
			return error;
		}

		// Get account balance
		try {
			Double myBalance = bank.getAccountBalance(customerID, fromaccountType);
			out.println("Please enter the amount to move. The available balance is : " + myBalance);
			amount = in.readLine();
			if (amount == null || amount.isEmpty() || amount.trim().isEmpty()) {
				out.println("No amount entered, aborting.");
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

		if (fromaccountType != null && toaccountType != null) {
			return "MOVE " + amount + " " + fromaccountType + " " + toaccountType;
		}
		return error;
	}

	private String paymentBuilder(String customerID) { // Method by M.Christou
		String accountType = "Main";
		String amount;
		String beneficiary;
		// Get beneficiary
		try {
			out.println("Please enter the beneficiary to pay:");
			beneficiary = in.readLine();
			if (beneficiary == null || beneficiary.isEmpty() || beneficiary.trim().isEmpty()) {
				out.println("Beneficiary is empty, aborting.");
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

		return "PAY " + beneficiary + " " + amount;
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
			String password = in.readLine();
			out.println("Checking Details...");
			// authenticate user and get customer ID token from bank for use in subsequent
			// requests
			String customerID = bank.checkLogInDetails(userName, password);
			// if the user is authenticated then get requests from the user and process them
			if (customerID != null) {
				clearScreen();
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
							clearScreen();
							request = "SHOWMYACCOUNTS";
							break;
						case "2":
							clearScreen();
							String accountType = accountCreation();
							request = "NEWACCOUNT " + accountType;
							if (accountType.equals(error)) {
								out.println("Wrong account type selection!");
								validCommand = false;
								mainMenu();
							}
							break;
						case "3":
							clearScreen();
							request = paymentBuilder(customerID);
							if (request.equals(error)) {
								validCommand = false;
								mainMenu();
							}
							break;
						case "4":
							clearScreen();
							request = moveBuilder(customerID);
							if (request.equals(error)) {
								validCommand = false;
								mainMenu();
							}
							break;
						case "5":
						case "Logout":
							clearScreen();
							Thread.currentThread().interrupt();
							run();
							break;
						case "SHOWMYACCOUNTS":
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

package newbank.server;

import java.sql.SQLException;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class NewBank {

	private static final NewBank bank = new NewBank();
	private Integer minCreditScore = 75;
	private String lowInterestPeriod = "30";
	DatabaseHandler dbHandle = new DatabaseHandler();

	private NewBank() {
		try {
			dbHandle.connectDatabase();
			dbHandle.initiateDatabase();
			dbHandle.addTestData();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static NewBank getBank() {
		return bank;
	}

	public synchronized String checkLogInDetails(String userName, String password) {
		try {
			return dbHandle.checkLogInDetails(userName, password);
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return null;
	}

	public synchronized String getCustomerID(String userName) {// Method implemented by M. Christou
		try {
			return dbHandle.getCustomerID(userName);
		} catch (SQLException e) {
			e.printStackTrace();
			return null;
		}
	}

	public synchronized boolean userNameExists(String username) {// Method implemented by M. Christou
		try {
			if (dbHandle.getCustomerID(username) != null) {
				return true;
			}
		} catch (SQLException e) {
			e.printStackTrace();
			return false;
		}
		return false;
	}

	// commands from the NewBank customer are processed in this method
	public synchronized String processRequest(String customerID, String request) {
		String[] command = request.split(" ");
		try {
			if (dbHandle.customerExists(customerID)) {
				switch (command[0]) {
					case "SHOWMYACCOUNTS":
						return showMyAccounts(customerID);
					// Added by M. Christou
					case "SHOWACTIVELOANINFO":
						return showActiveLoanInfo(customerID);
					// Added by H. Chan
					case "NEWACCOUNT":
						return newAccount(customerID, command[1]);
					// Added by M. Christou
					case "MOVE":
						return transferMoney(customerID, command[2], command[3], command[1]);
					// Added by M. Christou
					case "PAY":
						return pay(command[1], customerID, command[2]);
					case "CHANGEPASS":
						return changePassword(customerID, command[2], command[3], command[4]);
					case "GETCURRENTSALT":
						return getCurrentSalt(customerID);
					case "LOAN": // Y.Canli + M.Christou
						return newLoan(customerID, command[1], command[2]);
					case "REPAYLOAN": // M.Christou
						return repayLoan(customerID, command[1]);
					default:
						return "FAIL : No such command.";
				}
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return "FAIL";
	}

	private String getCurrentSalt(String customerID) { // Method implemented by M. Christou
		try {
			return dbHandle.getCurrentSalt(customerID);
		} catch (SQLException e) {
			e.printStackTrace();
			return null;
		}
	}

	private String showMyAccounts(String customerID) { // Method implemented by M. Christou
		try {
			return dbHandle.showMyAccounts(customerID);
		} catch (Exception e) {
			return "Cannot display accounts";
		}

	}

	private String repayLoan(String customerID, String amount) { // Method implemented by M. Christou
		Double amountToRepay = null;
		Boolean moneyDeducted = false;
		// Valid amount
		try {
			amountToRepay = Double.parseDouble(amount);
		} catch (Exception e) {
			return "Amount entered is not valid.";
		}

		// Check if customer has enough money - deduct it
		try {
			if (transaction(customerID, "Main", -amountToRepay).equals("SUCCESS")) {
				moneyDeducted = true;
			}
		} catch (Exception e) {
			return "Cannot deduct money from account. Please check your balance or contact the bank.";
		}

		// Update loan balance
		try {
			String dbStringReturn = dbHandle.modifyLoanBalance(customerID, amountToRepay);
			if ((dbStringReturn.equals("Loan repaid") && Boolean.TRUE.equals(moneyDeducted))
					|| (dbStringReturn.startsWith("Loan repayment successfull.") && Boolean.TRUE.equals(moneyDeducted))) {
				moneyDeducted = false;
				return dbStringReturn;
			}
			if (dbStringReturn.equals("Repay amount is more than outstanding amount") && Boolean.TRUE.equals(moneyDeducted)) {
				transaction(customerID, "Main", amountToRepay);
				moneyDeducted = false;
				return dbStringReturn;
			}
			if (dbStringReturn.equals("Error : No active loan") && Boolean.TRUE.equals(moneyDeducted)) {
				transaction(customerID, "Main", amountToRepay);
				moneyDeducted = false;
				return dbStringReturn;
			}
		} catch (Exception e) {
			transaction(customerID, "Main", amountToRepay);
			moneyDeducted = false;
			return "Please contact the bank with error code LR001";
		}
		return "Please contact the bank with error code LR002";
	}

	private String showActiveLoanInfo(String customerID) { // Method implemented by H. Chan
		try {
			return dbHandle.activeLoanInformation(customerID);
		} catch (Exception e) {
			return "Cannot display accounts";
		}

	}

	private String newAccount(String customerID, String accountType) { // Method implemented by M. Christou
		try {
			if (Boolean.FALSE.equals(dbHandle.accountExists(customerID, accountType))) {
				return dbHandle.createAccount(customerID, accountType);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return "Account already exists";
	}

	public double getAccountBalance(String customerID, String accountType) {// Method implemented by M.Christou
		try {
			return dbHandle.getAccountBalance(customerID, accountType);
		} catch (Exception e) {
			System.out.println("Error in getting account balance");
			e.printStackTrace();
			return 0.00;
		}
	}

	private String pay(String beneficiary, String customerID, String amount) { // Method by M.Christou
		// We should have 3 seperate strings here.
		double payment;
		String beneficiaryID = null;
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

	private String transaction(String customerID, String accountType, Double amount) { // Method implemented by M.Christou
		try {
			return dbHandle.modifyAccountBalance(customerID, accountType, amount);
		} catch (SQLException e) {
			e.printStackTrace();
			return "FAIL";
		}
	}

	public Boolean accountExists(String customerID, String accountType) {// Method implemented by M.Christou
		try {
			return dbHandle.accountExists(customerID, accountType);
		} catch (Exception e) {
			return false;
		}
	}

	private String transferMoney(String customerID, String fromaccountType, String toaccountType, String amount) { // Method
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

	public String changePassword(String customerID, String oldPassHash, String newPassHash, String salt) { // Method
																																																					// implemented
																																																					// by M.
																																																					// Christou
		return dbHandle.changePassword(customerID, oldPassHash, newPassHash, salt);
	}

	public String newLoan(String customerID, String loanAmount, String loanPeriodDays) {

		// Check if user is eligible for loan.
		try {
			if (Boolean.TRUE.equals(dbHandle.customerHasActiveLoan(customerID))) {
				return "Error : You already have an active loan.";
			}
			int creditscore = Integer.parseInt(dbHandle.getCreditScore(customerID));
			if (creditscore <= minCreditScore) {
				return "Error : Your credit score is " + creditscore + ". The minimum score is " + minCreditScore;
			}
			if (Boolean.FALSE.equals(dbHandle.accountExists(customerID, "Main"))) {
				return "Error : You do not have an active Main account. ";
			}

		} catch (Exception e) {
			e.printStackTrace();
			return "Error: Please contact the bank with error code L-001";
		}

		// Confirm with the user the terms of loan
		Double interestRate = 20.0;
		if (Integer.parseInt(loanPeriodDays) <= Integer.parseInt(lowInterestPeriod)) {
			interestRate = 15.0;
		}
		Double payable = Double.parseDouble(loanAmount) * (1 + interestRate / 100);
		payable = (double) (Math.round(payable) * 100.0 / 100.0);
		Double loanAmountDouble = (double) (Math.round(Double.parseDouble(loanAmount)) * 100.0 / 100.0);
		try {
			String timeStamp = new SimpleDateFormat("yyyy MM dd HH mm").format(new Date());
			if (Boolean.TRUE
					.equals(dbHandle.createLoan(customerID, timeStamp + " C:" + customerID, loanAmountDouble.toString(), payable.toString(),
							payable.toString(), loanPeriodDays , "ACTIVE"))
					&&
					dbHandle.modifyAccountBalance(customerID, "Main", loanAmountDouble).equals("SUCCESS")) {
						DecimalFormat df = new DecimalFormat("###.00");
					return "Success. You have been granted a loan of $" + df.format(loanAmountDouble) + " repayable in " + loanPeriodDays
							+ " days. The interest rate is " +
							interestRate + "%. Total amount repayable is $: " + df.format(payable);
			}
		} catch (Exception e) {
			e.printStackTrace();
			return "Error: Please contact the bank with error code L-002";
		}
		return "Error: Please contact the bank with error code L-003";
	}

}

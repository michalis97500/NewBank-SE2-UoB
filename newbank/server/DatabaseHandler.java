package newbank.server;

import java.sql.Timestamp;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Date;
import java.util.Objects;

public class DatabaseHandler {
  private Connection databaseConnection;
  private static String noaccount = "NOACCOUNT";
  private static String main = "Main";
  private static String savings = "Savings";
  private static String checking = "Checking";
  private static String accountTable = "Account_Table";
  private static String loanTable = "Loan_Table"; // Ycanli string for loan_table
  private static String updateAllAccountInfo = "INSERT INTO " + accountTable
      + "(id, username, passhash, salt, name, Main, Savings, Checking, CreditScore) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)";
  private static String updateAllLoanInfo = "INSERT INTO " + loanTable // ycanli + updated by M.Christou
      + "(id, loan_id, Loan_Amount, Total_Repayment, Outstanding, Loan_Days ,Completed) VALUES (?, ?, ?, ?, ?, ?, ?)";

  public Boolean connectDatabase() throws SQLException { // Method implemented by M.Christou
    try {
      if (databaseConnection != null && databaseConnection.isValid(2)) {
        return true;
      }
    } catch (Exception e) {
      System.out.print("No active connection");
      e.printStackTrace();
      return false;
    }

    try {
      Class.forName("org.sqlite.JDBC");
      databaseConnection = DriverManager.getConnection("jdbc:sqlite:BankDatabase.db");
      System.out.println("Database driver connected");
      return true;
    } catch (Exception e) {
      System.out.print("Database connection failed");
      e.printStackTrace();
      return false;
    }
  }

  public void initiateDatabase() { // Method implemented by M.Christou
    try (Statement tablecreation = databaseConnection.createStatement()) {
      String accountHeaders = "CREATE TABLE IF NOT EXISTS " + accountTable + " (\n"
          + "id text PRIMARY KEY ,\n"
          + "username text NOT NULL ,\n"
          + "passhash text NOT NULL ,\n"
          + "salt text NOT NULL ,\n"
          + "name text NOT NULL ,\n"
          + "Main text ,\n"
          + "Savings text ,\n"
          + "Checking text ,\n"
          + "CreditScore text NOT NULL\n"
          + ");";
      tablecreation.execute(accountHeaders);
      System.out.println("Account_Table has been created or it already exists");
    } catch (Exception e) {
      System.out.println("Account_Table cannot be created");
      e.printStackTrace();
    }

    try (Statement tablecreation = databaseConnection.createStatement()) { // ycanli + updated by M.Christou
      String loanHeaders = "CREATE TABLE IF NOT EXISTS " + loanTable + " (\n"
          + "id text NOT NULL,\n"
          + "loan_id text PRIMARY KEY,\n"
          + "Loan_Amount text NOT NULL,\n"
          + "Total_Repayment  text NOT NULL,\n"
          + "Outstanding text NOT NULL,\n"
          + "Loan_Days text NOT NULL,\n"
          + "Completed text NOT NULL \n"
          + ");";
      tablecreation.execute(loanHeaders);
      System.out.println("Loan_table has been created or it already exists");

    } catch (Exception e) {
      System.out.println("Loan_table cannot be created");
      e.printStackTrace();
    }
  }

  public void updateAccountInfo(String customerID, String username, String passhash, String salt, String name,
      String main, String savings, String checking, String creditScore) { // Method implemented by M.Christou
    try (PreparedStatement ps = this.databaseConnection.prepareStatement(updateAllAccountInfo)) {
      ps.setString(1, customerID);
      ps.setString(2, username);
      ps.setString(3, passhash);
      ps.setString(4, salt);
      ps.setString(5, name);
      ps.setString(6, main);
      ps.setString(7, savings);
      ps.setString(8, checking);
      ps.setString(9, creditScore);
      ps.executeUpdate();
      System.out.println("Table account_info updated, new values added: ");
      System.out
          .print(customerID + "\t" + username + "\t" + passhash + "\t" + salt + "\t" + name + "\t" + main + "\t"
              + savings + "\t" + checking + "\t" + creditScore);
      System.out.println();

    } catch (SQLException e) {
      System.out.println("Table account_info NOT updated, new values NOT added");
      System.out.print(customerID + "\t" + username + "\t" + passhash + "\t" + salt + "\t" + name + "\t" + main + "\t"
          + savings + "\t" + checking + "\t" + creditScore);
      System.out.println();
      e.printStackTrace();
    }
  }

  public Boolean setCustomerAccountBalance(String customerID, String accountType, String amount) throws SQLException { // Method
                                                                                                                       // implemented
                                                                                                                       // by
                                                                                                                       // M.
                                                                                                                       // Christou
    try (PreparedStatement ps = this.databaseConnection.prepareStatement(
        "UPDATE " + accountTable + " SET " + accountType + "='" + amount + "' WHERE id = " + customerID)) {
      ps.executeUpdate();
      System.out.println("Amount set successfully.");
      return true;
    } catch (SQLException e) {
      System.out.println("Unable to set amount");
      e.printStackTrace();
      return false;
    }
  }

  public Boolean accountActive(String account) { // Method implemented by M. Christou
    return (account != null && !account.equals(noaccount) && !account.equals("") && !account.equals(" "));
  }

  public Boolean accountExists(String customerID, String accountType) throws SQLException {// Method implemented by M.
                                                                                           // Christou
    Statement statement = databaseConnection.createStatement();
    String accountbalance = null;
    try {
      String idAndBalances = "SELECT id, " + accountType + " FROM " + accountTable;
      ResultSet results = statement.executeQuery(idAndBalances);
      Boolean customerFound = false;
      while (results.next()) {
        if (customerID.equals(results.getString("id"))) { // if we are here that means we have the correct customer
          accountbalance = results.getString(accountType);
          customerFound = true;
          break;
        }
      }
      if (Boolean.FALSE.equals(customerFound)) {
        return false;
      }
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    } finally {
      statement.close();
    }

    return (Boolean.TRUE.equals(accountActive(accountbalance)));

  }

  public String showMyAccounts(String customerID) throws SQLException {// Method implemented by M. Christou
    Statement statement = databaseConnection.createStatement();
    String mainBalance = null;
    String checkingBalance = null;
    String savingsBalance = null;

    // Get accounts from database
    try {
      String idAndBalances = "SELECT id, Main, Savings, Checking FROM " + accountTable;
      ResultSet results = statement.executeQuery(idAndBalances);
      while (results.next()) {
        if (customerID.equals(results.getString("id"))) { // if we are here that means we have the correct customer
          mainBalance = results.getString(main);
          checkingBalance = results.getString(checking);
          savingsBalance = results.getString(savings);
          break;
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
      return "No database connection";
    } finally {
      statement.close();
    }

    // Put accounts in a string to return
    StringBuilder accounts = new StringBuilder();
    try {
      if (Boolean.TRUE.equals(accountActive(mainBalance))) {
        accounts.append("Main : $" + mainBalance + "\n");
      }
      if (Boolean.TRUE.equals(accountActive(checkingBalance))) {
        accounts.append("Checking : $" + checkingBalance + "\n");
      }
      if (Boolean.TRUE.equals(accountActive(savingsBalance))) {
        accounts.append("Savings : $" + savingsBalance + "\n");
      }
      return accounts.toString();
    } catch (Exception e) {
      e.printStackTrace();
      return "Cannot display account details";
    }

  }

  public boolean customerExists(String customerID) throws SQLException {// Method implemented by M. Christou
    Statement statement = databaseConnection.createStatement();
    try {
      String idAndBalances = "SELECT id FROM " + accountTable;
      ResultSet results = statement.executeQuery(idAndBalances);
      while (results.next()) {
        if (customerID.equals(results.getString("id"))) { // if we are here that means we have the correct customer
          return true;
        }
      }
      return false;
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    } finally {
      statement.close();
    }
  }

  public String checkLogInDetails(String username, String password) throws SQLException {// Method implemented by M.
                                                                                         // Christou
    Statement statement = databaseConnection.createStatement();
    try {
      String userName = "SELECT id, username, passhash FROM " + accountTable;
      ResultSet results = statement.executeQuery(userName);
      while (results.next()) {
        if (username.equals(results.getString("username")) && password.equals(results.getString("passhash"))) { // if we
                                                                                                                // are
                                                                                                                // here
                                                                                                                // that
                                                                                                                // means
                                                                                                                // we
                                                                                                                // have
                                                                                                                // the
                                                                                                                // correct
                                                                                                                // customer
          return results.getString("id");
        }
      }
      return null;
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    } finally {
      statement.close();
    }
  }

  public String getCustomerID(String username) throws SQLException {// Method implemented by M. Christou
    Statement statement = databaseConnection.createStatement();
    try {
      String idAndBalances = "SELECT id, username FROM " + accountTable;
      ResultSet results = statement.executeQuery(idAndBalances);
      while (results.next()) {
        if (username.equals(results.getString("username"))) { // if we are here that means we have the correct customer
          return results.getString("id");
        }
      }
      return null;
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    } finally {
      statement.close();
    }
  }

  public String getCurrentSalt(String customerID) throws SQLException {// Method implemented by M. Christou
    Statement statement = databaseConnection.createStatement();
    try {
      String idAndSalt = "SELECT id, salt FROM " + accountTable;
      ResultSet results = statement.executeQuery(idAndSalt);
      while (results.next()) {
        if (customerID.equals(results.getString("id"))) { // if we are here that means we have the correct customer
          return results.getString("salt");
        }
      }
      return null;
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    } finally {
      statement.close();
    }
  }

  public String createAccount(String customerID, String accountType) throws SQLException {// Method implemented by M.
                                                                                          // Christou
    try (Statement statement = databaseConnection.createStatement()) {
      if (accountType.equals(main) || accountType.equals(checking) || accountType.equals(savings)) {
        String idAndAccount = "SELECT id, " + accountType + " FROM " + accountTable;
        ResultSet results = statement.executeQuery(idAndAccount);
        while (results.next()) {
          if (results.getString("id").equals(customerID)) {
            Boolean setBalance = setCustomerAccountBalance(customerID, accountType, "0.00");
            if (Boolean.TRUE.equals(setBalance)) {
              return "Account created successfully";
            }
            return "Could not configure new account.";
          }
        }
        return "Customer not found";
      }
      return "Wrong account type";
    } catch (Exception e) {
      e.printStackTrace();
      return "FAIL";
    }
  }

  public Double getAccountBalance(String customerID, String accountType) throws SQLException {// Method implemented by
                                                                                              // M. Christou
    try (Statement statement = databaseConnection.createStatement()) {
      if (Boolean.TRUE.equals(accountExists(customerID, accountType))) {
        String idAndAccount = "SELECT id, " + accountType + " FROM " + accountTable;
        ResultSet results = statement.executeQuery(idAndAccount);
        while (results.next()) {
          if (results.getString("id").equals(customerID)) {
            return Double.parseDouble(results.getString(accountType));
          }

        }
      }
      return 0.00;
    } catch (Exception e) {
      e.printStackTrace();
      return 0.00;
    }
  }

  public String modifyAccountBalance(String customerID, String accountType, Double amount) throws SQLException {// Method
                                                                                                                // implemented
                                                                                                                // by M.
                                                                                                                // Christou
    Double currentBalance = 0.00;
    Double newAmount = 0.00;
    try {
      currentBalance = getAccountBalance(customerID, accountType);
      newAmount = amount + currentBalance;

    } catch (Exception e) {
      return "FAIL";
    }
    String newAmountString = newAmount.toString();
    try (PreparedStatement ps = this.databaseConnection.prepareStatement(
        "UPDATE " + accountTable + " SET " + accountType + "='" + newAmountString + "' WHERE id = " + customerID)) {
      ps.executeUpdate();
      return "SUCCESS";
    } catch (SQLException e) {
      e.printStackTrace();
      return "FAIL";
    }
  }

  public void addTestData() {// Method implemented by M. Christou
    updateAccountInfo("1", "Bhagy", "$2a$10$RkrdW3pxOvLIZlTV0kfiiuo8zSshxC3RofVWDiHukv37uabGRoylq",
        "$2a$10$RkrdW3pxOvLIZlTV0kfiiu", "Bhagy", noaccount, noaccount, noaccount,
        "0");
    updateAccountInfo("2", "John", "$2a$10$RkrdW3pxOvLIZlTV0kfiiuo8zSshxC3RofVWDiHukv37uabGRoylq",
        "$2a$10$RkrdW3pxOvLIZlTV0kfiiu", "John", "100", "50", "2500", "60");
    updateAccountInfo("3", "Test", "$2a$10$RkrdW3pxOvLIZlTV0kfiiuo8zSshxC3RofVWDiHukv37uabGRoylq",
        "$2a$10$RkrdW3pxOvLIZlTV0kfiiu", "Test", noaccount, "999999", noaccount,
        "100");
    updateAccountInfo("4", "Test2", "$2a$10$RkrdW3pxOvLIZlTV0kfiiuo8zSshxC3RofVWDiHukv37uabGRoylq",
        "$2a$10$RkrdW3pxOvLIZlTV0kfiiu", "Test2", noaccount, "999999", noaccount,
        "100");
  }

  public String changePassword(String customerID, String oldPassHash, String newPassHash, String salt) { // Method
                                                                                                         // implemented
                                                                                                         // by M.
                                                                                                         // Christou
    Boolean oldPassCorrect = false;
    Boolean passHashChanged = false;
    try (Statement statement = databaseConnection.createStatement()) {
      if (Boolean.TRUE.equals(customerExists(customerID))) {
        String idAndPassHash = "SELECT id, passhash  FROM " + accountTable;
        ResultSet results = statement.executeQuery(idAndPassHash);
        while (results.next()) {
          if (results.getString("id").equals(customerID) && results.getString("passhash").equals(oldPassHash)) {
            oldPassCorrect = true;
          }
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
      return "Error, please contact the bank with error code PC-001";
    }

    // update passHash
    try (PreparedStatement ps = this.databaseConnection.prepareStatement(
        "UPDATE " + accountTable + " SET passhash ='" + newPassHash + "' WHERE id = " + customerID)) {
      if (Boolean.TRUE.equals(oldPassCorrect)) {
        ps.executeUpdate();
        passHashChanged = true;
      }
    } catch (Exception e) {
      e.printStackTrace();
      return "Error, please contact the bank with error code PC-002";
    }

    // update Salt
    try (PreparedStatement ps = this.databaseConnection.prepareStatement(
        "UPDATE " + accountTable + " SET salt ='" + salt + "' WHERE id = " + customerID)) {
      if (Boolean.TRUE.equals(oldPassCorrect) && Boolean.TRUE.equals(passHashChanged)) {
        ps.executeUpdate();
        return "Password has been changed.";
      }
      return "Error, please contact the bank with error code PC-003";

    } catch (Exception e) {
      e.printStackTrace();
      return "Error, please contact the bank with error code PC-002";
    }

  }

  public boolean createLoan(String id, String loanID, String loanAmount, String totalDebt,
      String outstanding, String loanDays, String completed) { // Method implemented by ycanli + modified by M.Christou
    try {
      if (Boolean.TRUE.equals(customerHasActiveLoan(id))) {
        return false;
      }
    } catch (SQLException e1) {
      e1.printStackTrace();
    }

    try (PreparedStatement ps = this.databaseConnection.prepareStatement(updateAllLoanInfo)) {
      ps.setString(1, id);
      ps.setString(2, loanID);
      ps.setString(3, loanAmount);
      ps.setString(4, totalDebt);
      ps.setString(5, outstanding);
      ps.setString(6, loanDays);
      ps.setString(7, completed);
      ps.executeUpdate();
      System.out
          .print(
              id + "\t" + loanID + "\t" + loanAmount + "\t" + totalDebt + "\t" + outstanding + "\t" + loanDays + "\t"
                  + completed + "\t");
      return true;
    } catch (SQLException e) {
      System.out.println("FALSE");
      e.printStackTrace();
      return false;
    }
  }

  public Boolean customerHasActiveLoan(String customerID) throws SQLException {// Method implemented by ycanli +
                                                                               // modified by M.Christou
    Statement statement = databaseConnection.createStatement();
    try {
      String idAndLoan = "SELECT id, Completed FROM " + loanTable;
      ResultSet results = statement.executeQuery(idAndLoan);
      while (results.next()) {
        if (customerID.equals(results.getString("id")) && results.getString("Completed").equals("ACTIVE")) { // if we
                                                                                                             // are here
                                                                                                             // that
                                                                                                             // means we
                                                                                                             // have the
                                                                                                             // correct
                                                                                                             // loan
          return true;
        }
      }
      return false;
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    } finally {
      statement.close();
    }
  }

  public String activeLoanInformation(String customerID) throws SQLException {// Implemented by M.Christou
    Statement statement = databaseConnection.createStatement();
    try {
      String idAndLoan = "SELECT id, Completed, Outstanding, Total_Repayment, loan_id, Loan_Days FROM " + loanTable;
      ResultSet results = statement.executeQuery(idAndLoan);
      while (results.next()) {
        if (customerID.equals(results.getString("id")) && results.getString("Completed").equals("ACTIVE")) {
          String loanID = results.getString("loan_id");
          String[] loanIDArray = loanID.split("C:");

          SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy MM dd HH mm");
          Date parsedDate = dateFormat.parse(loanIDArray[0]);
          Timestamp timestamp = new Timestamp(parsedDate.getTime());
          Date date = new Date(timestamp.getTime());

          Calendar cal = Calendar.getInstance();
          cal.setTime(date);
          cal.add(Calendar.DATE, Integer.parseInt(results.getString("Loan_Days")));
          date = cal.getTime();

          return "Outstanding amount : $" + results.getString("Outstanding") + "\n" + "Total Repayment : $"
              + results.getString("Total_Repayment") + "\n" +
              "Loan due date : " + date;
        }
      }
      return "No active loan found";
    } catch (Exception e) {
      e.printStackTrace();
      return "No database connection";
    } finally {
      statement.close();
    }
  }

  public String getCreditScore(String customerID) throws SQLException {// Method implemented by ycanli + modified by
                                                                       // M.Christou
    Statement statement = databaseConnection.createStatement();
    try {
      String idAndScore = "SELECT id, CreditScore FROM " + accountTable;
      ResultSet results = statement.executeQuery(idAndScore);
      while (results.next()) {
        if (customerID.equals(results.getString("id"))) { // if we are here that means we have the correct score
          return results.getString("CreditScore");
        }
      }
      return null;
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    } finally {
      statement.close();
    }
  }

  public String modifyLoanBalance(String customerID, Double amount) throws SQLException {
    // Method implemented by M.Christou
    Statement statement = databaseConnection.createStatement();
    try {
      String idAndLoan = "SELECT id, loan_id , Outstanding, Completed FROM " + loanTable;
      ResultSet results = statement.executeQuery(idAndLoan);
      while (results.next()) {
        if (customerID.equals(results.getString("id")) && results.getString("Completed").equals("ACTIVE")) {
          Double outStanding = Double.parseDouble(results.getString("Outstanding"));
          Double currentAmount = outStanding - amount;
          if (Objects.equals(currentAmount, 0.0)) {
            Boolean repaid = setLoanComplete(results.getString("loan_id"));
            if (Boolean.TRUE.equals(repaid)) {
              return "Loan repaid";
            }
            return "Error : Please contact the bank with code LR004";
          }

          if (currentAmount < 0) {
            return "Repay amount is more than outstanding amount";
          }
          if (currentAmount > 0) {
            Boolean repaid = setLoanAmount(results.getString("loan_id"), currentAmount);
            if (Boolean.TRUE.equals(repaid)) {
              return "Loan repayment successfull. New outstanding amount : " + (currentAmount);
            }
          }
        }
      }
      return "Error : No active loan";
    } catch (Exception e) {
      e.printStackTrace();
      return null;
    } finally {
      statement.close();
    }
  }

  public Boolean setLoanComplete(String loanID) throws SQLException {
    // Method implemented by M.Christou
    try (PreparedStatement ps = this.databaseConnection.prepareStatement(
        "UPDATE " + loanTable + " SET Completed='COMPLETE' WHERE loan_id = '" + loanID + "'")) {
      ps.executeUpdate();
      return true;
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }

  public Boolean setLoanAmount(String loanID, Double amount) throws SQLException {
    // Method implemented by M.Christou
    try (PreparedStatement ps = this.databaseConnection.prepareStatement(
        "UPDATE " + loanTable + " SET Outstanding='" + amount.toString() + "' WHERE loan_id = '" + loanID + "'")) {
      ps.executeUpdate();
      return true;
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }

}
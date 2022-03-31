package newbank.server;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class Customer {

	private ArrayList<Account> accounts;

	public Customer() {
		accounts = new ArrayList<>();
	}

	public String accountsToString() {
		String s = "";
		for (Account a : accounts) {
			s += a.toString();
		}
		return s;
	}

	public Map<String, Double> getAccounts() {
		Map<String, Double> map = new HashMap<>();
		for (Account a : accounts) {
			map.put(a.getAccountName(), a.getAccountValue());
		}
		return map;
	}

	public void addAccount(Account account) {
		accounts.add(account);
	}

	public String modifyBalance(String account, Double transaction) {
		for (Account a : accounts) {
			if (a.getAccountName().equals(account)) {
				a.modifyBalance(transaction);
				return "SUCCESS";
			}
		}
		return "FAIL";
	}
}

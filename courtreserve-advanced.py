import logging
import time
import os
# import pdb
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException

if os.path.exists(".env"):
    try:
        from dotenv import load_dotenv
        load_dotenv()
    except ImportError:
        logging.warning("python-dotenv not installed; ensure env variables are set")

# ── CONFIG ───────────────────────────────────────────────────────────
USERNAME         = os.getenv("USERNAME")
PASSWORD         = os.getenv("PASSWORD")
LOGIN_URL        = "https://app.courtreserve.com/account/login"
SUCCESS_PATH     = "/Online/Portal/Index"
PB_EVENTS_URL    = "https://app.courtreserve.com/Online/Events/List/9175?evTypeId=30892"
DASHBOARD_URL    = "https://app.courtreserve.com/Online/Portal/Index/9175?forceDashboard=True"
WAIT_TIMEOUT     = 10    # seconds for explicit waits
MAX_RETRIES      = 6
RETRY_DELAY      = 30     # seconds between retries

BROWSER_VISIBLE = False
# ───────────────────────────────────────────────────────────────────

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s"
)

def login_with_selenium():
    options = webdriver.ChromeOptions()
    if not BROWSER_VISIBLE:
        options.add_argument("--headless=new")
        options.add_argument("--window-size=1920,1080")
        options.add_argument("--disable-blink-features=AutomationControlled")
        options.add_argument("--user-agent=Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
    
    options.add_experimental_option("excludeSwitches", ["enable-automation"])
    options.add_experimental_option('useAutomationExtension', False)
    
    driver = webdriver.Chrome(options=options)
    driver.execute_script("Object.defineProperty(navigator, 'webdriver', {get: () => undefined})")

    # small implicit wait to smooth out quick re-renders
    driver.implicitly_wait(2)
    wait = WebDriverWait(driver, WAIT_TIMEOUT)

    try:
        # 1) Log in
        driver.get(LOGIN_URL)
        logging.info("Opening login page")
        user_in = wait.until(EC.visibility_of_element_located((By.ID, "Username")))
        pass_in = wait.until(EC.visibility_of_element_located((By.ID, "Password")))
        user_in.clear(); user_in.send_keys(USERNAME)
        pass_in.clear(); pass_in.send_keys(PASSWORD, Keys.RETURN)

        # 2) Wait for dashboard
        wait.until(EC.url_contains(SUCCESS_PATH))
        logging.info("Logged in successfully")

        # 3) Navigate to events page
        driver.get(PB_EVENTS_URL)
        wait.until(EC.url_contains("/Online/Events/List"))
        logging.info("Events list loaded")

        # 4) Filter to Today
        today_label = wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, "label[for='dates-1']")))
        time.sleep(3)  # brief pause for animation
        today_label.click()
        logging.info("Filtered to Today")

        # 5) Click the first "Register"
        xpath_register = (
            "//*[(self::a or self::button) and "
            "translate(normalize-space(.),"
            "'ABCDEFGHIJKLMNOPQRSTUVWXYZ',"
            "'abcdefghijklmnopqrstuvwxyz')='register']"
        )
        retries = 0
        while retries < MAX_RETRIES:
            try:
                reg = wait.until(EC.element_to_be_clickable((By.XPATH, xpath_register)))
                reg.click()
                logging.info("Clicked first Register")
                break
            except TimeoutException:
                retries += 1
                logging.warning(f"No Register found (attempt {retries}/{MAX_RETRIES}), retrying in {RETRY_DELAY}s…")
                time.sleep(RETRY_DELAY)
                driver.refresh()
                wait.until(EC.url_contains("/Online/Events/List"))
                wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, "label[for='dates-1']"))).click()
        else:
            logging.error("Failed to find Register after retries, exiting")
            return

        # 6) Click a second "Register" if it appears
        try:
            reg2 = wait.until(EC.element_to_be_clickable((By.XPATH, xpath_register)))
            time.sleep(3)
            reg2.click()
            logging.info("Clicked second Register")
        except TimeoutException:
            logging.info("No second Register element found") 

        # 7) Check the box for Niamh
        label = wait.until(EC.element_to_be_clickable((
            By.XPATH,
            "//label[@for and normalize-space(text())='Niamh Mac Namara']"
        )))
        label.click()
        logging.info("Checked Niamh Mac Namara")

        # 8) Finalize registration
        finalize_btn = wait.until(EC.element_to_be_clickable((
            By.XPATH,
            "//button[normalize-space(text())='Finalize Registration']"
        )))
        finalize_btn.click()
        logging.info("Clicked Finalize Registration")

        # 9) Wait for dashboard redirect, or fallback to 3s wait
        try:
            wait.until(EC.url_to_be(DASHBOARD_URL))
            logging.info("Redirected to forced dashboard")
        except TimeoutException:
            logging.warning("Dashboard redirect not detected, waiting 3 seconds")
            time.sleep(3)

    finally:
        driver.quit()

if __name__ == "__main__":
    login_with_selenium()
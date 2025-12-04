import logging
import time
import os
# import pdb
from selenium import webdriver
from datetime import date, timedelta, datetime, timezone
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
# UPDATED: Removed ?evTypeId=30892 to show ALL events (Morning, Intermediate, etc.)
PB_EVENTS_URL    = "https://app.courtreserve.com/Online/Events/List/9175"
DASHBOARD_URL    = "https://app.courtreserve.com/Online/Portal/Index/9175?forceDashboard=True"
WAIT_TIMEOUT     = 10    # seconds for explicit waits
MAX_RETRIES      = 10
RETRY_DELAY      = 1     # seconds between retries
dt = date.today() + timedelta(weeks=1)
NEXT_WEEK_DATE = f"{dt.strftime('%b')} {dt.day}"
REGISTER_HOUR_UTC    = 11
REGISTER_MINUTE_UTC  = 0
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
        user_in = wait.until(EC.visibility_of_element_located((By.NAME, "email")))
        pass_in = wait.until(EC.visibility_of_element_located((By.NAME, "password")))
        user_in.clear(); user_in.send_keys(USERNAME)
        pass_in.clear(); pass_in.send_keys(PASSWORD, Keys.RETURN)

        # 2) Wait for dashboard
        wait.until(EC.url_contains(SUCCESS_PATH))
        logging.info("Logged in successfully")

        # 3) Navigate to events page
        driver.get(PB_EVENTS_URL)
        wait.until(EC.url_contains("/Online/Events/List"))
        logging.info("Events list loaded")

        # if it's before REGISTER_HOUR_UTC, sleep until that time in UTC —
        now_utc = datetime.now(timezone.utc)
        target = now_utc.replace(
            hour=REGISTER_HOUR_UTC,
            minute=REGISTER_MINUTE_UTC,
            second=0,
            microsecond=0
        )
        # if target rolled backwards (e.g. now is past the target), skip sleep
        if now_utc < target:
            wait_seconds = (target - now_utc).total_seconds()
            logging.info(
                f"Current UTC time is {now_utc.strftime('%H:%M:%S')}, "
                f"before {REGISTER_HOUR_UTC:02d}:{REGISTER_MINUTE_UTC:02d} UTC — "
                f"sleeping {int(wait_seconds)}s until then"
            )
            time.sleep(wait_seconds)

        logging.info(f"Searching for Register button for date {NEXT_WEEK_DATE}")
        fast_wait = WebDriverWait(driver, 2)

        retries = 0
        while retries < MAX_RETRIES:
            try:
                # Robust Selector Logic:
                # 1. Find all event containers
                # 2. Find the one matching NEXT_WEEK_DATE
                # 3. Find the Register button inside it
                
                # We use a single XPath to do this efficiently but robustly
                # Find a .fj_post that contains the date AND has a Register button
                # Note: We use normalize-space to handle whitespace weirdness
                
                # Construct XPath to find the specific button
                # Matches: Container -> Date Link -> ... -> Register Button
                robust_xpath = (
                    f"//div[contains(@class,'fj_post')]"
                    f"[.//span[@class='title-part']//a[contains(normalize-space(.),'{NEXT_WEEK_DATE}')]]"
                    "//a[normalize-space(text())='Register']"
                )
                
                reg = fast_wait.until(EC.element_to_be_clickable((By.XPATH, robust_xpath)))
                driver.execute_script("arguments[0].click();", reg)
                logging.info("Clicked first Register")
                break
            except TimeoutException:
                retries += 1
                logging.warning(f"No Register found (attempt {retries}/{MAX_RETRIES}), retrying in {RETRY_DELAY}s…")
                time.sleep(RETRY_DELAY)
                driver.refresh()
                wait.until(EC.url_contains("/Online/Events/List"))
                # Try to clear filters if they exist, or just wait for list
                # wait.until(EC.element_to_be_clickable((By.CSS_SELECTOR, "label[for='dates-1']"))).click() 
                # ^ Commented out filter click as we want DEFAULT view now
        else:
            logging.error("Failed to find Register after retries, exiting")
            return

        # 6) Click a second "Register" if it appears
        try:
            second_register_xpath = (
                "//div[contains(@class,'details-action-buttons')]"
                "//a[contains(@class,'btn-register') and normalize-space(text())='Register']"
            )
            reg2 = wait.until(EC.element_to_be_clickable((By.XPATH, second_register_xpath)))
            driver.execute_script("arguments[0].click();", reg2)
            logging.info("Clicked second Register")
        except TimeoutException:
            logging.info("No second Register element found") 

        # 7) Check the box for Niamh and Niels
        try:
            labelNiamh = wait.until(EC.element_to_be_clickable((
                By.XPATH,
                "//label[@for and normalize-space(text())='Niamh Mac Namara']"
            )))
            labelNiamh.click()
            logging.info("Checked Niamh Mac Namara")
        except:
            logging.warning("Could not check Niamh (maybe not found)")

        try:
            labelNiels = wait.until(EC.element_to_be_clickable((
                By.XPATH,
                "//label[@for and normalize-space(text())='Niels Van der Velden']"
            )))
            labelNiels.click()
            logging.info("Checked Niels Van der Velden")
        except:
            logging.warning("Could not check Niels (maybe not found)")

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
    try:
        login_with_selenium()
    except Exception as e:
        logging.error(f"First run failed with error: {e}")
        logging.info("Retrying entire script once more...")
        login_with_selenium()

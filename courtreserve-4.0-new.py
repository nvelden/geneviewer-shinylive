import logging
import time
import os
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
PB_EVENTS_URL    = "https://app.courtreserve.com/Online/Events/List/9175?evTypeId=41411"
DASHBOARD_URL    = "https://app.courtreserve.com/Online/Portal/Index/9175?forceDashboard=True"
WAIT_TIMEOUT     = 10
MAX_RETRIES      = 10
RETRY_DELAY      = 1
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

def login_and_scrape():
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

    driver.implicitly_wait(2)
    wait = WebDriverWait(driver, WAIT_TIMEOUT)

    try:
        # 1) Log in
        driver.get(LOGIN_URL)
        logging.info("Opening login page")
        user_in = wait.until(EC.visibility_of_element_located((By.NAME, "email")))
        pass_in = wait.until(EC.visibility_of_element_located((By.NAME, "password")))
        user_in.clear()
        user_in.send_keys(USERNAME)
        pass_in.clear()
        pass_in.send_keys(PASSWORD, Keys.RETURN)

        # 2) Wait for dashboard
        wait.until(EC.url_contains(SUCCESS_PATH))
        logging.info("Logged in successfully")

        # 3) Navigate to events page
        driver.get(PB_EVENTS_URL)
        wait.until(EC.url_contains("/Online/Events/List"))
        logging.info("Events list loaded")

        # 4) SCRAPE DATA FIRST
        result = scrape_event_dates_and_urls(driver, wait)

        # 5) THEN OPTIONALLY WAIT FOR REGISTRATION TIME
        now_utc = datetime.now(timezone.utc)
        target = now_utc.replace(
            hour=REGISTER_HOUR_UTC,
            minute=REGISTER_MINUTE_UTC,
            second=0,
            microsecond=0
        )
        if now_utc < target:
            wait_seconds = (target - now_utc).total_seconds()
            logging.info(
                f"Current UTC time is {now_utc.strftime('%H:%M:%S')}, "
                f"before {REGISTER_HOUR_UTC:02d}:{REGISTER_MINUTE_UTC:02d} UTC — "
                f"sleeping {int(wait_seconds)}s until then"
            )
            time.sleep(wait_seconds)

        # 6) After wait, navigate to the event page for NEXT_WEEK_DATE
        matching_urls = []
        for date_text, url in result:
            if NEXT_WEEK_DATE in date_text:
                matching_urls.append(url)

        if matching_urls:
            logging.info(f"Found {len(matching_urls)} events matching {NEXT_WEEK_DATE}.")
            # navigate to the first one
            first_url = matching_urls[0]
            event_url_full = f"https://app.courtreserve.com{first_url}"
            driver.get(event_url_full)
            logging.info(f"Navigated to event page: {event_url_full}")

            # 7) Retry logic for Register button
            second_register_xpath = (
                "//div[contains(@class,'details-action-buttons')]"
                "//a[contains(@class,'btn-register') and normalize-space(text())='Register']"
            )

            fast_wait = WebDriverWait(driver, 4)
            retries = 0
            while retries < MAX_RETRIES:
                try:
                    reg2 = fast_wait.until(EC.element_to_be_clickable((By.XPATH, second_register_xpath)))
                    button_text = reg2.text.strip()
                    driver.execute_script("arguments[0].click();", reg2)
                    logging.info(f"Clicked button: [{button_text}]")
                    break
                except TimeoutException:
                    retries += 1
                    logging.warning(
                        f"No suitable Register-related button found "
                        f"(attempt {retries}/{MAX_RETRIES}), retrying in {RETRY_DELAY}s…"
                    )
                    time.sleep(RETRY_DELAY)
                    driver.refresh()
            else:
                logging.error("Failed to find second Register button after retries, exiting.")
                return

            # 8) Check the box for Niels
            member_label = wait.until(EC.element_to_be_clickable((
                By.XPATH,
                "//table[@id='family-table-grid']"
                "//tr[.//label[normalize-space()='Niels Van der Velden']]"
                "//label[@for='CurrentMember_IsChecked']"
            )))
            member_label.click()
            logging.info("Checked Niels Van der Velden")

            # 9) Finalize registration
            finalize_btn = wait.until(EC.element_to_be_clickable((
                By.XPATH,
                "//button[normalize-space(text())='Finalize Registration']"
            )))
            finalize_btn.click()
            logging.info("Clicked Finalize Registration")

            # 10) Wait for dashboard redirect, or fallback to 3s wait
            try:
                wait.until(EC.url_to_be(DASHBOARD_URL))
                logging.info("Redirected to forced dashboard")
            except TimeoutException:
                logging.warning("Dashboard redirect not detected, waiting 3 seconds")
                time.sleep(3)

        else:
            logging.warning(f"No events found for {NEXT_WEEK_DATE}.")

        return result

    finally:
        driver.quit()

def scrape_event_dates_and_urls(driver, wait):
    """
    Scrape all event dates and data-urls from the page.
    """
    rows = driver.find_elements(By.CSS_SELECTOR, ".col-lg-12.fn-event-item")
    result = []
    for row in rows:
        try:
            details = row.find_element(By.CSS_SELECTOR, ".details")
            data_url = details.get_attribute("data-url")

            date_elem = details.find_element(
                By.XPATH,
                ".//span[contains(@class, 'icon-title-row')]//span[contains(@class, 'title-part')]/a"
            )
            date_text = date_elem.text.strip().replace("\n", " ")
            result.append((date_text, data_url))
        except Exception as e:
            logging.warning(f"Skipping row due to error: {e}")
            continue
    return result

if __name__ == "__main__":
    data = login_and_scrape()

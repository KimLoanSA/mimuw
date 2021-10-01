import {Builder, By, Capabilities, Key, until} from 'selenium-webdriver';
import { expect } from 'chai';
import { driver } from 'mocha-webdriver';

const host: string = 'http://localhost:3000';
const loginUrl: string = `${host}/static/user/login.html`;
const passwordChangeUrl: string = `${host}/static/user/passwordChange.html`;

const quizUrl: string = `${host}/static/quiz/quiz.html`;
const quizEndingUrl: string = `${host}/static/quiz/quizEnding.html`;
const quizErrorUrl: string = `${host}/static/quiz/quizAlreadySolvedError.html`;

const user1Username: string = 'user1';
const user1Password: string = 'user1';

const user2Username: string = 'user2';
const user2Password: string = 'user2';

describe('basic functions tests', () => {

  it('login', async function () {
    this.timeout(2000);
    await loginUser1();

    expect(await driver.getCurrentUrl()).to.equal(quizUrl);
  });

  it('saving quiz results', async function () {
    this.timeout(20000);
    await loginUser1();

    await driver.find('#start-quiz-button').doClick();

    await answerOnQuestion(79);
    await answerOnQuestion(11712);
    await answerOnQuestion(324);
    await answerOnQuestion(42);

    await (await driver.find('#quiz-question-navigation-stop-button')).doClick();

    expect(await driver.getCurrentUrl()).to.equal(quizEndingUrl);
    expect(await driver.find('#quiz-ending-stats-table-answers').getText()).to.equal('4 / 4');
  });

  it('solves quiz again', async function() {
    this.timeout(20000);

    await loginUser2();

    await driver.executeScript('window.open()');
    const tabs = await driver.getAllWindowHandles();

    // filling in 1. tab
    await driver.switchTo().window(tabs[0]);
    await driver.get(quizUrl);
    await driver.find('#start-quiz-button').doClick();

    await answerOnQuestion(79);
    await answerOnQuestion(11712);
    await answerOnQuestion(324);
    await answerOnQuestion(42);

    // filling in 2. tab
    await driver.switchTo().window(tabs[1]);
    await driver.get(quizUrl);
    await driver.find('#start-quiz-button').doClick();

    await answerOnQuestion(79);
    await answerOnQuestion(11712);
    await answerOnQuestion(324);
    await answerOnQuestion(42);

    await (await driver.find('#quiz-question-navigation-stop-button')).doClick();
    expect(await driver.getCurrentUrl()).to.equal(quizEndingUrl);

    await driver.switchTo().window(tabs[0]);
    await (await driver.find('#quiz-question-navigation-stop-button')).doClick();
    expect(await driver.getCurrentUrl()).to.equal(quizErrorUrl);
  });

  it('password change', async function () {
      this.timeout(20000);

      await loginUser1();

      const cookies = await driver.manage().getCookies();

      await driver.manage().deleteAllCookies();

      // should be logged out
      await driver.get(quizUrl);
      expect(await driver.getCurrentUrl()).to.equal(loginUrl);

      await loginUser1();
      expect(await driver.getCurrentUrl()).to.equal(quizUrl);

      // reset password
      await (await (await driver.find('#quiz-header-password-change-button')).doClick());
      expect(await driver.getCurrentUrl()).to.equal(passwordChangeUrl);

      await changePassword(user1Password, 'test123');
      expect(await driver.getCurrentUrl()).to.equal(loginUrl);

      await driver.manage().deleteAllCookies();
      cookies
        .forEach(async cookie => await driver.manage().addCookie(cookie));

      await driver.get(quizUrl);
      expect(await driver.getCurrentUrl()).to.equal(loginUrl);

      await login(user1Username, 'test123');
      expect(await driver.getCurrentUrl()).to.equal(quizUrl);
    });

});

async function loginUser1() {
  await login(user1Username, user1Password);
}

async function loginUser2() {
  await login(user2Username, user2Password);
}

async function login(name: string, password: string) {
  await driver.get(loginUrl);

  await driver.find('input[name=username]').sendKeys(name);
  await driver.find('input[name=password]').sendKeys(password);
  await driver.find('input[type=submit]').doClick();
}

async function answerOnQuestion(answer: number) {
  await (await driver.find('#quiz-question-answer-input')).doSendKeys(answer.toString());
  await (await driver.find('#quiz-question-navigation-next-button')).doClick();
}

async function changePassword(oldPassword: string, newPassword: string) {
  await driver.find('input[name=oldPassword]').sendKeys(oldPassword);
  await driver.find('input[name=newPassword]').sendKeys(newPassword);
  await driver.find('input[name=newPasswordConfirmation]').sendKeys(newPassword);
  await driver.find('input[type=submit]').doClick();
}

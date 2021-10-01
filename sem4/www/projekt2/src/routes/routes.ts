import { Router } from 'express';
import UserRouter from './userRoute';
import QuizRouter from './quizRoute'

const router = Router();

router.use('/user', UserRouter);
router.use('/quiz', QuizRouter);

export default router;

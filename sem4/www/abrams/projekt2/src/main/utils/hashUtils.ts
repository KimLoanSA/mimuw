import bcrypt from 'bcrypt'

const saltingRounds = 10;


export function hashPassword(password: string): string {
  const salt = bcrypt.genSaltSync(saltingRounds);

  return bcrypt.hashSync(password, salt);
}


export function comparePasswordWithHash(password: string, passwordHash: string): boolean {
  return bcrypt.compareSync(password, passwordHash);
}

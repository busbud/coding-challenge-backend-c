import {ValidationArguments, ValidatorConstraint, ValidatorConstraintInterface} from 'class-validator';

@ValidatorConstraint({name: '', async: false})
export class LongitudeValidator implements ValidatorConstraintInterface {
    validate(longitude: string, args: ValidationArguments) {
        const longitudeFloat = parseFloat(longitude);
        return longitudeFloat >= -180.00 && longitudeFloat <= 180.00;
    }

    defaultMessage(args: ValidationArguments) {
        return '($value) has to be between -180 and 180 !';
    }
}

import {ValidationArguments, ValidatorConstraint, ValidatorConstraintInterface} from 'class-validator';

@ValidatorConstraint({name: '', async: false})
export class LatitudeValidator implements ValidatorConstraintInterface {

    validate(latitude: string, args: ValidationArguments) {
        let latitudeFloat = parseFloat(latitude);
        return latitudeFloat >= -90.00 && latitudeFloat <= 90.00; //
    }

    defaultMessage(args: ValidationArguments) {
        return '($value) has to be between -90 and 90 !';
    }
}

package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @ClassName: CustomerServiceDTO
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/5/20 10:29
 * @Version: 1.0
 */
@Data
@ApiModel("修改联系客服")
public class CustomerServiceDTO {

    @ApiModelProperty("联系客服热线")
    private String customerServiceHotLine;

    @ApiModelProperty("邮箱地址")
    private String eMail;
}

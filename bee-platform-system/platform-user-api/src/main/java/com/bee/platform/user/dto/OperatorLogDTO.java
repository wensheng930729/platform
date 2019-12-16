package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @description: 操作日志实体
 * @author: junyang.li
 * @create: 2019-05-05 10:01
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("操作日志实体")
public class OperatorLogDTO implements Serializable {

    private static final long serialVersionUID = -4992194566410241939L;

    @ApiModelProperty("主键")
    private Integer id;

    @ApiModelProperty("操作人id")
    private Integer operatorId;

    @ApiModelProperty("操作人名称")
    private String operatorName;

    @ApiModelProperty("操作人角色")
    private Integer operatorRoleId;

    @ApiModelProperty("操作人角色名称")
    private String operatorRoleName;

    @ApiModelProperty("操作内容")
    private String operatorContent;

    @ApiModelProperty("操作时间")
    private Date operatorTime;
}

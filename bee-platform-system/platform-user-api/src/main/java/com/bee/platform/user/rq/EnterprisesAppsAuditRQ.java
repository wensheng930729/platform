package com.bee.platform.user.rq;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("审核产品角色申请的参数")
public class EnterprisesAppsAuditRQ implements Serializable {

	private static final long serialVersionUID = 1L;
 
    @ApiModelProperty("申请单id")
    @NotNull(message = "id不能为空")
    private Integer id;
    
    @ApiModelProperty("审核状态（0-未通过，1-通过）")
    @NotNull(message = "审核意见不能为空")
    private Integer auditState;
    
    @ApiModelProperty("拒绝原因")
    private String rejectReason;
}

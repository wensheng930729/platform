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
@ApiModel("查询企业审核申请参数")
public class EnterprisesCheckRQ implements Serializable {
	private static final long serialVersionUID = 1L;
	
    @ApiModelProperty("审核状态（0-未通过，1-通过，2-未审核，3-全部）")
    @NotNull(message = "审核状态不能为空")
    private Integer auditState;

    @ApiModelProperty("搜索内容-企业名称")
    private String content;

}

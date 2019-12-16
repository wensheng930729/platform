package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @ClassName EnterpriseStatusDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/3/18$ 11:03$
 * @version 1.0.0
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "企业审核状态信息信息")
public class EnterpriseCheckStatusDTO {

    @ApiModelProperty("未处理的企业数量（我的待办数量）")
    private Integer untreatedCount;

    @ApiModelProperty("已通过企业数量（已通过企业）")
    private Integer passedCount;

    @ApiModelProperty("申请企业总数（申请总数）")
    private Integer sumCount;

}

package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName EnterprisesRelationUserRQ
 * @Description 功能描述
 * @Date 2019/5/5 9:10
 **/
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "企业关联用户请求参数")
public class EnterprisesRelationUserRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("企业名称")
    @NotEmpty(message = "企业名称不能为空")
    private String enterpriseName;

    @ApiModelProperty("申请理由")
    @NotEmpty(message = "申请理由不能为空")
    @Length(max = 100,message = "昵称限制100个字符")
    private String applyReason;


}

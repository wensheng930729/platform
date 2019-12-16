package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @ClassName SystemCodeDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/24$ 16:53$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("角色码表信息")
public class SystemCodeDTO implements Serializable {

    private static final long serialVersionUID = -3792639954046571839L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("组id（不唯一）")
    private String sysGroupId;

    @ApiModelProperty("该组id下面的key(唯一)")
    private String sysCode;

    @ApiModelProperty("该码值的说明x信息")
    private String sysCodeDesc;

    @ApiModelProperty("角色级别")
    private Integer level;

    @ApiModelProperty("创建时间")
    private Date createTime;

}

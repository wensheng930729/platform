package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName AppListDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/4/30$ 11:35$
 * @version 1.0.0
 */

@Data
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业产品列表信息")
public class AppListDTO implements Serializable {

    private static final long serialVersionUID = -6608355614580376424L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("appId")
    private Integer appId;

    @ApiModelProperty("产品名称")
    private String name;

    @ApiModelProperty("产品缩写")
    private String abbreviation;
}

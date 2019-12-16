package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * @ClassName: HelperContentDTO
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/4/30 14:35
 * @Version: 1.0
 */

@Getter
@Setter
@ToString
@NoArgsConstructor
@ApiModel(value = "帮助首页编辑大类列表中的实体类")
public class HelperContentDTO {

    @ApiModelProperty("大小类关联表主键id")
    private Integer refId;

    @ApiModelProperty("小类主键标识id")
    private Integer sId;

    @ApiModelProperty("小类名称")
    private String sName;

    @ApiModelProperty("文章内容主键id")
    private Integer cId;

    @ApiModelProperty("文章标题")
    private String cName;

    @ApiModelProperty("帮助首页下的显示排序")
    private Integer helperWeights;
}

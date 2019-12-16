package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.time.LocalDateTime;

/**
 * @ClassName: SecondaryClassification
 * @Description: 系统帮助下小类实体类
 * @Author: fei.sun
 * @Date: 2019/4/28 11:43
 * @Version: 1.0
 */

@Getter
@Setter
@ToString
@Accessors(chain=true)
@NoArgsConstructor
@TableName("m_secondary_classification")
@ApiModel("系统帮助下小类")
public class SecondaryClassification {

    @ApiModelProperty("小类主键标识id")
    @TableId(value = "id",type = IdType.AUTO)
    private Integer id;

    @ApiModelProperty("小类名称")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;

    @ApiModelProperty("大类关联id")
    private Integer pId;

    /*@ApiModelProperty("分类类型（1 代表常见问题大类，2代表用户指南大类）")
    private Integer classifyType;

    @ApiModelProperty("是否为帮助首页下的小类（0否，1是）")
    private Integer isHelperClassify;

    @ApiModelProperty("帮助首页下的小类显示排序数值，当不是帮助首页下的小类时该值为空（预留字段）")
    private Integer helperWeights;*/

    @ApiModelProperty("状态标识，0代表使用中，1代表删除")
    private Integer status;

    @ApiModelProperty("创建时间")
    private LocalDateTime createTime;

    @ApiModelProperty("更新时间")
    private LocalDateTime updateTime;

    @ApiModelProperty("创建人id")
    private Long createUid;

    @ApiModelProperty("更新人id")
    private Long updateUid;
}

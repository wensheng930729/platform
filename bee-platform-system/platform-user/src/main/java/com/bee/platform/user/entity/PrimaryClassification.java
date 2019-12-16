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
import org.springframework.format.annotation.DateTimeFormat;

import java.time.LocalDateTime;

/**
 * @ClassName: PrimaryClassification
 * @Description: 大类分类
 * @Author: fei.sun
 * @Date: 2019/4/28 9:55
 * @Version: 1.0
 */
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain=true)
@TableName("m_primary_classification")
@ApiModel(value = "系统帮助中的大类")
public class PrimaryClassification {

    @ApiModelProperty("大类主键标识id")
    @TableId(value = "id",type = IdType.AUTO)
    private Integer id;

    @ApiModelProperty("大类名称")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;

    @ApiModelProperty("大类分类，0代表帮助指南大类，1代表常见问题大类，2代表用户指南大类")
    private Integer classifyType;

    @ApiModelProperty("状态标识，0代表使用中，1代表删除")
    private Integer status;

    @ApiModelProperty("创建时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime createTime;

    @ApiModelProperty("更新时间")
    private LocalDateTime updateTime;

    @ApiModelProperty("创建人id")
    private Long createUid;

    @ApiModelProperty("更新人id")
    private Long updateUid;
}

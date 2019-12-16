package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.annotations.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @ClassName: NewsType
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/4/25 14:13
 * @Version: 1.0
 */
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@TableName("news_type")
@ApiModel(value = "资讯类型")
public class NewsType {

    /**
     * 资讯类型id
     */
    @ApiModelProperty("资讯类型id")
    private Long id;
    /**
     * 类型名称
     */
    @ApiModelProperty("资讯类型名称")
    private String typeName;
}

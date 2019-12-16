package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @description: 新闻返回对象
 * @author: junyang.li
 * @create: 2019-03-22 18:54
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class NewDTO implements Serializable {

    private static final long serialVersionUID = -3084808999743403476L;
    @ApiModelProperty("新闻数据")
    List<NewDetailDTO> content;

    @ApiModelProperty("总页数 ")
    Integer totalPages;

    @ApiModelProperty("数据总数")
    Integer totalElements;

    @ApiModelProperty("每页条数 ")
    Integer size;

    Integer number;
}

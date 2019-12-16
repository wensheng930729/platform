package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName ErpLogoDTO
 * @Description erp附件DTO
 * @Date 2019-5-28
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "erp附件DTO")
public class ErpLogoDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @ApiModelProperty("附件名称")
    private String  name;

    @ApiModelProperty("附件url")
    private String url;




}

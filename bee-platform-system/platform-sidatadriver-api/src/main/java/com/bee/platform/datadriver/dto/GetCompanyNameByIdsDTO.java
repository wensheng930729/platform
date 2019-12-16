package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName GetCompanyNameByIdsDTO
 * @Description 功能描述
 * @Date 2019/6/19 11:26
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("根据ids获取公司名称发布会信息")
public class GetCompanyNameByIdsDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;


}

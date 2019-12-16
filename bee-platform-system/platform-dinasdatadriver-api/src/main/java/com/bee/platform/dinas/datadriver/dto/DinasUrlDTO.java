package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 采购结算表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("附件url返回参数")
public class DinasUrlDTO implements Serializable {

    private String uid;
    private String name;
    private String status;
    private String url;
}
